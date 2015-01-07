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

unit Forms.PropertyItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Forms.Dialog.BaseEntity, VirtualTrees,
  Vcl.ExtCtrls, Vcl.StdCtrls, DKLang, Frame.BaseEntity, NodeDataTypes.Custom;

type
  TfrmPropertyItem = class(TfrmDialogBase)
    DKLanguageController1: TDKLanguageController;
  private
    { Private declarations }
    FListNodeData: TvCustomRealNodeData;
  strict protected
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; ANodeData: TvCustomRealNodeData);

    class function Execute(AOwner: TComponent; ANodeData: TvCustomRealNodeData): Integer;
  end;

var
  frmPropertyItem: TfrmPropertyItem;

implementation

uses
  Frame.Properties.Advanced, Kernel.Enumerations,
  Frame.Properties.Behavior, Frame.Properties.General.Category,
  Frame.Properties.General.Software;

{$R *.dfm}

{ TfrmPropertyItem }

constructor TfrmPropertyItem.Create(AOwner: TComponent;
  ANodeData: TvCustomRealNodeData);
begin
  FListNodeData := ANodeData;
  inherited Create(AOwner);
end;

class function TfrmPropertyItem.Execute(AOwner: TComponent;
  ANodeData: TvCustomRealNodeData): Integer;
var
  frm: TfrmPropertyItem;
begin
  Result := mrCancel;
  frm := TfrmPropertyItem.Create(AOwner, ANodeData);
  try
    Result := frm.ShowModal;
  finally
    frm.Free;
  end;
end;

function TfrmPropertyItem.InternalLoadData: Boolean;
begin
  Assert(Assigned(FListNodeData), 'FListNodeData is not assigned!');

  if (FListNodeData.DataType = vtdtFile) or (FListNodeData.DataType = vtdtFolder) then
    FFrameGeneral := AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmSWGeneralPropertyPage.Create(Self, FListNodeData)))
  else
    if FListNodeData.DataType = vtdtCategory then
      FFrameGeneral := AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmCatGeneralPropertyPage.Create(Self, FListNodeData)));
  AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmAdvancedPropertyPage.Create(Self, FListNodeData)));
  AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmBehaviorPropertyPage.Create(Self, FListNodeData)));
end;

function TfrmPropertyItem.InternalSaveData: Boolean;
begin
  FListNodeData.Changed := True;
  //Reset icon and get it again
  //TODO: Fix it (dmImages)
//  FListNodeData.ResetIcon(isAny);
//  ImagesDM.GetNodeImageIndex(FListNodeData, isAny);
end;

end.
