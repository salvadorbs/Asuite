{
Copyright (C) 2006-2015 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Scanner.Thread;

interface

uses
  Messages, SysUtils, Classes, VirtualTrees, Forms, Scanner.Folder, ComCtrls, StdCtrls,
  DKLang, Dialogs, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities;

type
  TScanThread = class(TThread)
    private
      FCancel : Boolean;
      FTree   : TBaseVirtualTree;
      FDialog : TForm;
      FScannerFolder : TScannerFolder;
      FParentNode : PVirtualNode;
      //Settings
      FAutoExtractName: Boolean;
      FFlat: Boolean;
    FShellTree: TVirtualExplorerTree;

      function CreateDialogProgressBar(DialogMsg: String): TForm;
      procedure DoDialogCancelClick(Sender: TObject);
      function GetAllCheckedFolders(ASender: TVirtualExplorerTree): TScannerFolder;
    protected
      procedure Execute; override;
    public
      property ParentNode: PVirtualNode read FParentNode write FParentNode;
      property Flat: Boolean read FFlat write FFlat;
      property AutoExtractName: Boolean read FAutoExtractName write FAutoExtractName;

      procedure CloseAndFreeDialog;
      procedure UpdateGUI;
      procedure Cancel;

      constructor Create(CreateSuspended: Boolean; ATree: TBaseVirtualTree; AShellTree: TVirtualExplorerTree); overload;
      destructor Destroy; override;
  end;

implementation

uses
  NodeDataTypes.Base, VirtualTree.Methods, AppConfig.Main, Kernel.Enumerations;

{ TScanThread }

procedure TScanThread.Cancel;
begin
  FCancel := True;
end;

procedure TScanThread.CloseAndFreeDialog;
begin
  FDialog.Close;
  FDialog.Free;
end;

constructor TScanThread.Create(CreateSuspended: Boolean; ATree: TBaseVirtualTree; AShellTree: TVirtualExplorerTree);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FCancel         := False;
  FTree           := ATree;
  FShellTree      := AShellTree;
  FScannerFolder  := GetAllCheckedFolders(AShellTree);
  FDialog         := CreateDialogProgressBar(DKLangConstW('msgScanningProgress'));
end;

function TScanThread.CreateDialogProgressBar(DialogMsg: String): TForm;
var
  ProgressBar: TProgressBar;
  btnCancel: TButton;
begin
  //Create Dialog and ProgressBar (in Dialog)
  Result         := CreateMessageDialog(DialogMsg, mtInformation, []) ;
  Result.Caption := DialogMsg;
  Result.Height  := 150;
  //Progress bar
  ProgressBar := TProgressBar.Create(Result) ;
  with ProgressBar do
  begin
    Name  := 'Progress';
    Parent := Result;
    Max   := FScannerFolder.Count;
    Step  := 1;
    Top   := 50;
    Left  := 8;
    Width := Result.ClientWidth - 16;
  end;
  //Button Cancel
  btnCancel := TButton.Create(Result);
  with btnCancel do
  begin
    Name  := 'btnCancel';
    Parent := Result;
    Top   := ProgressBar.Top + ProgressBar.Height + 10;
    Left  := 8;
    Width := Result.ClientWidth - 16;
  end;
  btnCancel.Caption := DKLangConstW('msgScanningCancel');
  btnCancel.Cancel  := True;
  btnCancel.OnClick := DoDialogCancelClick;
  Result.Show;
end;

destructor TScanThread.Destroy;
begin
  FreeAndNil(FScannerFolder);
  inherited;
end;

procedure TScanThread.DoDialogCancelClick(Sender: TObject);
begin
  Self.Cancel;
end;

procedure TScanThread.Execute;

  procedure ScanFolder(AFolder: TScannerFolder; AParentNode: PVirtualNode);
  var
    Node, ParentNode: PVirtualNode;
    NodeData: TvBaseNodeData;
    I: Integer;
  begin
    for I := 0 to AFolder.Count - 1 do
    begin
      if FFlat then
        AParentNode := FParentNode;

      Node := TVirtualTreeMethods.Create.AddChildNodeEx(Config.MainTree, AParentNode, amInsertAfter, vtdtCategory);
      NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Config.MainTree);
      NodeData.Name := AFolder[I].Name;

      //TODO: Process files and folders

      ScanFolder(AFolder[I], Node);

      //Check category. If haven't child, remove it
      if (Node.ChildCount = 0) And (NodeData.DataType = vtdtCategory) then
        FTree.DeleteNode(Node);
    end;
  end;
begin
  inherited;
  FTree.BeginUpdate;
  try
    if NOT FCancel then
      ScanFolder(FScannerFolder, FParentNode);
  finally
    TVirtualTreeMethods.Create.GetAllIcons(Config.MainTree);
    FTree.EndUpdate;
  end;
end;

function TScanThread.GetAllCheckedFolders(
  ASender: TVirtualExplorerTree): TScannerFolder;

  procedure RecurseStorage(S: TNodeStorage; CheckedFolderList: TScannerFolder);
  var
    NS: TNamespace;
    i: integer;
    Str: string;
    CheckType: TCheckType;
  begin
    NS := TNamespace.Create(S.AbsolutePIDL, nil);
    NS.FreePIDLOnDestroy := False;
    // Need to do this to get the real path to special folders
    Str := NS.NameForParsing;
    { The items must - be in the file system, a valid file or directory, have a }
    { full check     }
    if NS.FileSystem and (FileExistsW(Str) or WideDirectoryExists(Str) or WideIsDrive(Str)) then
    begin
      //Get proper check
      case S.Storage.Check.CheckState of
        csUncheckedNormal,
        csUncheckedPressed: CheckType := fctUnchecked;
        csCheckedNormal,
        csCheckedPressed: CheckType := fctChecked;
        csMixedNormal,
        csMixedPressed: CheckType := fctMixed;
      end;
      //Add in FolderList
      if CheckType <> fctUnchecked then
      begin
        if Assigned(CheckedFolderList) then
          CheckedFolderList := CheckedFolderList.AddItem(Str, CheckType)
        else begin
          Result := TScannerFolder.Create(str, CheckType);
          CheckedFolderList := Result;
        end;
      end;
    end;

    if Assigned(S.ChildNodeList) and (CheckType <> fctChecked) then
      for i := 0 to S.ChildNodeList.Count - 1 do
        RecurseStorage(S.ChildNodeList[i], CheckedFolderList);

    NS.Free;
  end;

begin
  Result := nil;
  //Add mixed checked folders
  RecurseStorage(ASender.Storage, Result);
end;

procedure TScanThread.UpdateGUI;
var
  ProgressBar: TProgressBar;
begin
  ProgressBar := TProgressBar(FDialog.FindComponent('Progress'));
  ProgressBar.Position := ProgressBar.Position + 1;
  FDialog.Update;
end;

end.
