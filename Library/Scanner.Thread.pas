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
  DKLang, Dialogs, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  NodeDataTypes.Base, IOUtils, StrUtils;

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

    function CreateDialogProgressBar(DialogMsg: String): TForm;
    procedure DoDialogCancelClick(Sender: TObject);
    procedure CheckEmptyCategory(ANode: PVirtualNode; ANodeData: TvBaseNodeData);
    procedure ScanFolder(AFolder: TScannerFolder; AParentNode: PVirtualNode); overload;
    procedure ScanFolder(AFolderPath: string; AParentNode: PVirtualNode; AOnlyFiles: Boolean); overload;
    function FindMatchText(Strings: TStrings; const Str: string): Integer;
  protected
    procedure Execute; override;
  public
    property ParentNode: PVirtualNode read FParentNode write FParentNode;
    property Flat: Boolean read FFlat write FFlat;
    property AutoExtractName: Boolean read FAutoExtractName write FAutoExtractName;

    procedure CloseAndFreeDialog;
    procedure UpdateGUI;
    procedure Cancel;

    constructor Create(CreateSuspended: Boolean; ATree: TBaseVirtualTree; AFolder: TScannerFolder); overload;
    destructor Destroy; override;
  end;

implementation

uses
  VirtualTree.Methods, AppConfig.Main, Kernel.Enumerations, Utility.FileFolder,
  NodeDataTypes.Files;

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

constructor TScanThread.Create(CreateSuspended: Boolean; ATree: TBaseVirtualTree; AFolder: TScannerFolder);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FCancel         := False;
  FTree           := ATree;
  FScannerFolder  := AFolder;
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
    Top   := 50;
    Left  := 8;
    Style := pbstMarquee;
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
  inherited;
end;

procedure TScanThread.DoDialogCancelClick(Sender: TObject);
begin
  Self.Cancel;
end;

procedure TScanThread.CheckEmptyCategory(ANode: PVirtualNode; ANodeData: TvBaseNodeData);
begin
  //Check category. If haven't child, remove it
  if (ANode.ChildCount = 0) and (ANodeData.DataType = vtdtCategory) then
    FTree.DeleteNode(ANode);
end;

procedure TScanThread.ScanFolder(AFolder: TScannerFolder; AParentNode: PVirtualNode);
var
  Node, ParentNode: PVirtualNode;
  NodeData: TvBaseNodeData;
  I: Integer;
  sFileExt, sPath: String;
begin
  if FCancel then
    Exit;

  for I := 0 to AFolder.Count - 1 do
  begin
    if FFlat then
      AParentNode := FParentNode;

    Node := TVirtualTreeMethods.Create.AddChildNodeEx(Config.MainTree, AParentNode, amInsertAfter, vtdtCategory, False);
    NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Config.MainTree);
    NodeData.Name := AFolder[I].Name;

    if AFolder[I].Count = 0 then
      ScanFolder(AFolder[I].Path, Node, AFolder[I].CheckType = fctMixed)
    else
      ScanFolder(AFolder[I], Node);

    CheckEmptyCategory(Node, NodeData);
  end;
end;

procedure TScanThread.Execute;
begin
  inherited;
  if FScannerFolder.Count > 0 then
  begin
    FTree.BeginUpdate;
    try
      if NOT FCancel then
        ScanFolder(FScannerFolder, FParentNode);
    finally
      TVirtualTreeMethods.Create.GetAllIcons(Config.MainTree);
      FTree.EndUpdate;
    end;
  end;
end;

function TScanThread.FindMatchText(Strings: TStrings;
  const Str: string): Integer;
begin
  for Result := 0 to Strings.Count-1 do
    if ContainsText(Str, Strings[Result]) then
      exit;
  Result := -1;
end;

procedure TScanThread.ScanFolder(AFolderPath: string;
  AParentNode: PVirtualNode; AOnlyFiles: Boolean);
var
  Node: PVirtualNode;
  NodeData: TvBaseNodeData;
  sFileExt, sShortName, sPath: String;
begin
  if FCancel then
    Exit;

  //Flat structure
  if FFlat then
    AParentNode := FParentNode;

  //Folders
  if Not AOnlyFiles then
  begin
    for sPath in TDirectory.GetDirectories(AFolderPath) do
    begin
      Node := TVirtualTreeMethods.Create.AddChildNodeEx(Config.MainTree, AParentNode, amInsertAfter, vtdtCategory, False);
      NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Config.MainTree);
      NodeData.Name := ExtractFileName(sPath);

      ScanFolder(sPath, Node, False);

      CheckEmptyCategory(Node, NodeData);
    end;
  end;

  //Files
  for sPath in TDirectory.GetFiles(AFolderPath) do
  begin
    sFileExt := ExtractFileExt(sPath);
    sShortName := ExtractFileName(sPath);

    if (Config.ScanFolderFileTypes.IndexOf(sFileExt) <> -1) and (FindMatchText(Config.ScanFolderExcludeNames, sShortName) = -1) then
    begin
      Node := TVirtualTreeMethods.Create.AddChildNodeEx(Config.MainTree, AParentNode, amInsertAfter, vtdtFile, False);
      NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Config.MainTree);
      //Name
      if FAutoExtractName then
        NodeData.Name := ExtractFileNameEx(sPath)
      else
        NodeData.Name := sShortName;
      //Path
      TvFileNodeData(NodeData).PathFile := sPath;
    end;
  end;
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
