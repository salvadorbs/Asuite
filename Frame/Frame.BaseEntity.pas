{
Copyright (C) 2006-2013 Matteo Salvi

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

unit Frame.BaseEntity;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees;

type
  TfrmBaseEntityPage = class(TFrame)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; virtual;
    function GetImageIndex: integer; virtual;
    function InternalLoadData: Boolean; virtual;
    function InternalSaveData: Boolean; virtual;

//    procedure PopulateVSTItemList(const ATree: TBaseVirtualTree;const ABaseItemsList: TBaseItemsList);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function SaveData: Boolean; virtual;
    property Title: string read GetTitle;
    property ImageIndex: integer read GetImageIndex;
  end;

TPageFrameClass = class of TfrmBaseEntityPage;

implementation



{$R *.dfm}

{ TfrmBaseEntityPage }

constructor TfrmBaseEntityPage.Create(AOwner: TComponent);
begin
  inherited;
  Self.InternalLoadData;
end;

function TfrmBaseEntityPage.GetImageIndex: integer;
begin
  Result := -1;
end;

function TfrmBaseEntityPage.GetTitle: string;
begin
  Result := '';
end;

function TfrmBaseEntityPage.InternalLoadData: Boolean;
begin
  Result := True;
end;

function TfrmBaseEntityPage.InternalSaveData: Boolean;
begin
  Result := True;
end;

//procedure TfrmBaseEntityPage.PopulateVSTItemList(const ATree: TBaseVirtualTree;
//  const ABaseItemsList: TBaseItemsList);
//var
//  I: Integer;
//  CurrentFileData : TvCustomRealNodeData;
//  NewNode         : PVirtualNode;
//  NewNodeData     : PTreeDataX;
//begin
//  ATree.BeginUpdate;
//  try
//    for I := 0 to ABaseItemsList.Count - 1 do
//    begin
//      CurrentFileData := ABaseItemsList[I];
//      if Assigned(CurrentFileData) then
//      begin
//        NewNode := ATree.AddChild(ATree.RootNode);
//        NewNodeData := ATree.GetNodeData(NewNode);
//        //Set pointers
//        NewNodeData.pNodeList := CurrentFileData.PNode;
//      end;
//    end;
//    //Check nodes path and get icons
//    CheckVisibleNodePathExe(ATree);
//    //TODO: Fix it
////ImagesDM.GetChildNodesIcons(ATree, ATree.RootNode, isAny);
//    //Auto columns width
//    TVirtualStringTree(ATree).Header.AutoFitColumns;
//  finally
//    ATree.EndUpdate;
//  end;
//end;

function TfrmBaseEntityPage.SaveData: Boolean;
begin
  Result := Self.InternalSaveData;
end;

end.
