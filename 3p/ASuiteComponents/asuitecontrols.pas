{
Copyright (C) 2013 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit ASuiteControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TASuiteEdit }

  TASuiteEdit = class(TCustomEdit)
  private
    { Private declarations }
    FPlaceHolder: String;
    procedure HidePlaceHolder;
    procedure SetPlaceHolder(value: String);
    procedure ShowPlaceHolder;
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property AutoSelected;
  published
    { Published declarations }
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PlaceHolder: String read FPlaceHolder write SetPlaceHolder;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I ASuiteControls_icon.lrs}
  RegisterComponents('ASuite',[TASuiteEdit]);
end;

{ TASuiteEdit }

procedure TASuiteEdit.SetPlaceHolder(value: String);
begin
  if value <> FPlaceHolder then
  begin
    FPlaceHolder := value;
    //If TASuiteEdit is focused, don't show placeholder
    if Self.Focused then
      Self.Text := ''
    else begin
      Self.Text := FPlaceHolder;
      Self.Font.Color := clBtnShadow;
    end;
  end;
end;

procedure TASuiteEdit.HidePlaceHolder;
begin
  if (Self.Text = FPlaceholder) then
  begin
    Self.Text := '';
    Self.Font.Color := clDefault;
  end;
end;

procedure TASuiteEdit.ShowPlaceHolder;
begin
  if Self.Text = '' then
  begin
    Self.Text := FPlaceHolder;
    Self.Font.Color := clBtnShadow;
  end;
end;

procedure TASuiteEdit.DoEnter;
begin
  inherited;
  HidePlaceHolder;
end;

procedure TASuiteEdit.DoExit;
begin
  inherited;
  ShowPlaceHolder;
end;

constructor TASuiteEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlaceHolder := '';
end;

end.
