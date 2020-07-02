{
Copyright (C) 2006-2020 Matteo Salvi

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

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, Forms.Dialog.BaseEntity, StdCtrls,
  NodeDataTypes.Custom, DefaultTranslator;

type
  TfrmPropertyItem = class(TfrmDialogBase)
    
  private
    { Private declarations }
    FListNodeData: TvCustomRealNodeData;
  strict protected
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; ANodeData: TvCustomRealNodeData); overload;

    class function Execute(AOwner: TComponent; ANodeData: TvCustomRealNodeData): Integer;
  end;

var
  frmPropertyItem: TfrmPropertyItem;

implementation

uses
  Frame.Properties.Advanced, Kernel.Enumerations, Kernel.Logger,
  Frame.Properties.Behavior, Frame.Properties.General.Category,
  Frame.Properties.General.Software;

{$R *.lfm}

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
  TASuiteLogger.Info('Opening form Property Item (%s)', [ANodeData.Name]);

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

  Result := True;

  if (FListNodeData.DataType = vtdtFile) or (FListNodeData.DataType = vtdtFolder) then
    FFrameGeneral := AddFrameNode(vstCategory, nil, TfrmSWGeneralPropertyPage.Create(Self, FListNodeData))
  else
    if FListNodeData.DataType = vtdtCategory then
      FFrameGeneral := AddFrameNode(vstCategory, nil, TfrmCatGeneralPropertyPage.Create(Self, FListNodeData));
  AddFrameNode(vstCategory, nil, TfrmAdvancedPropertyPage.Create(Self, FListNodeData));
  AddFrameNode(vstCategory, nil, TfrmBehaviorPropertyPage.Create(Self, FListNodeData));
end;

function TfrmPropertyItem.InternalSaveData: Boolean;
begin
  Result := True;

  FListNodeData.Changed := True;
  //Reset icon and get it again
  FListNodeData.Icon.ResetIcon;
end;

end.
