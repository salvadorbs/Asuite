{
Copyright (C) 2006-2021 Matteo Salvi

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

unit Frame.Properties.EnvironmentVars;

{$MODE DelphiUnicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  ComCtrls, ActnList, Frame.Properties.Base;

type

  { TfrmEnvironmentVars }

  TfrmEnvironmentVars = class(TfrmBasePropertyPage)
    actAdd: TAction;
    actDelete: TAction;
    actCopy: TAction;
    actPaste: TAction;
    ActionList1: TActionList;
    grpOthers: TGroupBox;
    tbButtons: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    vleVars: TValueListEditor;
    procedure actAddExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    function FindEmptyRow: Integer;
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public

  end;

implementation

uses
  DataModules.Icons, Kernel.ResourceStrings, Clipbrd, Kernel.Consts,
  NodeDataTypes.Files, Kernel.Manager;

{$R *.lfm}

{ TfrmEnvironmentVars }

procedure TfrmEnvironmentVars.actAddExecute(Sender: TObject);
var
  EmptyRow: Integer;
begin
  EmptyRow := FindEmptyRow;

  //If there is not empty row, insert a new row else select empty row
  if EmptyRow = -1 then
    vleVars.InsertRow('', '', True)
  else
    vleVars.Row := EmptyRow + vleVars.FixedRows;

  vleVars.SetFocus;
end;

procedure TfrmEnvironmentVars.actCopyExecute(Sender: TObject);
var
  RowIndex: Integer;
begin
  RowIndex := vleVars.Row - vleVars.FixedRows;
  if (vleVars.Strings.Count > RowIndex) then
    Clipboard.AsText := vleVars.Strings[RowIndex];
end;

procedure TfrmEnvironmentVars.actDeleteExecute(Sender: TObject);
begin
  if vleVars.Row <> -1 then
    vleVars.DeleteRow(vleVars.Row);
end;

procedure TfrmEnvironmentVars.actPasteExecute(Sender: TObject);
begin
  vleVars.Strings.Insert(0, Clipboard.AsText);
end;

procedure TfrmEnvironmentVars.actPasteUpdate(Sender: TObject);
begin
  if Assigned(Sender) then
    TAction(Sender).Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TfrmEnvironmentVars.FrameResize(Sender: TObject);
begin
  //Workaround Win32 - Align right border with vleVars
  //TODO: Check this in QT and GTK2
  vleVars.Constraints.MaxWidth := vleVars.Width - 1;
  tbButtons.Constraints.MinWidth := vleVars.Width;
end;

function TfrmEnvironmentVars.FindEmptyRow: Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to vleVars.Strings.Count - 1 do
  begin
    if vleVars.Strings[I] = EmptyStr then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TfrmEnvironmentVars.GetTitle: string;
begin
  Result := msgEnvironmentVars;
end;

function TfrmEnvironmentVars.GetImageIndex: Integer;
begin
  Result := ASuiteManager.IconsManager.GetIconIndex('environment_vars');
end;

function TfrmEnvironmentVars.InternalLoadData: Boolean;
begin
  Result := inherited;

  //Align right border with vleVars
  tbButtons.Constraints.MinWidth := vleVars.Width + 1;

  tbButtons.Images      := dmImages.ilIcons;
  tbButtons.ImagesWidth := ICON_SIZE_SMALL;

  actAdd.ImageIndex    := ASuiteManager.IconsManager.GetIconIndex('add');
  actDelete.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('delete');
  actCopy.ImageIndex   := ASuiteManager.IconsManager.GetIconIndex('copy');
  actPaste.ImageIndex  := ASuiteManager.IconsManager.GetIconIndex('paste');

  vleVars.Strings.Clear;
  vleVars.Strings.AddStrings(TvFileNodeData(CurrentNodeData).EnvironmentVars);
end;

function TfrmEnvironmentVars.InternalSaveData: Boolean;
var
  EmptyRow: Integer;
  FileNodeData: TvFileNodeData;
begin
  Result := inherited;

  if CurrentNodeData is TvFileNodeData then
  begin
    FileNodeData := TvFileNodeData(CurrentNodeData);

    EmptyRow := FindEmptyRow;
    if EmptyRow <> -1 then
      vleVars.DeleteRow(EmptyRow + vleVars.FixedRows);

    FileNodeData.EnvironmentVars.Clear;
    FileNodeData.EnvironmentVars.AddStrings(vleVars.Strings);
  end;
end;

end.

