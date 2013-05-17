{   Component(s):
    tcyBaseContainer

    Description:
    Component base for dynamic form container.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyBaseContainer;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Forms, Graphics, SysUtils, Controls, Types, Grids, Messages;

type
  TEnterKeyAction = (enterKeyDefault, enterKeyNothing, enterKeyGotoNextControl);

  TcyCustomGrid = class(TCustomGrid)
  end;

  TcyBaseContainer = class(TComponent)
  private
    FSaveVisible: Boolean;
    FSaveAlign: TAlign;
    FSaveWidth: Integer;
    FSaveHeight: Integer;
    FSaveTop: Integer;
    FSaveLeft: Integer;
    FSaveParent: TWinControl;
    //
    FActive: Boolean;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FCaption: TCaption;
    FControl: TControl;
    FPosition: TPosition;
    {$IFDEF DELPHI2009_OR_ABOVE}
    FAlphaBlend: Boolean;
    FAlphaBlendValue: byte;
    FTransparentColor: Boolean;
    FTransparentColorValue: TColor;
    {$ENDIF}
    FWindowState: TWindowState;
    FOnClose: TNotifyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnShow: TNotifyEvent;
    FFormContainer: TForm;
    FShowHint: Boolean;
    FOnResize: TNotifyEvent;
    FEnterKeyAction: TEnterKeyAction;
    FOnDeactivate: TNotifyEvent;
    procedure SetBorderStyle(Value: TFormBorderStyle);
    procedure SetBorderIcons(Value: TBorderIcons);
    procedure SetControl(const Value: TControl);
    procedure OnFormShow(Sender: TObject);
    procedure OnFormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnFormDeactivate(Sender: TObject);
    procedure OnFormResize(Sender: TObject);
    procedure OnFormKeyPress(Sender: TObject; var Key: Char);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Active: Boolean read FActive default false;
    property BorderStyle: TFormBorderStyle read FBorderStyle write SetBorderStyle  default bsNone;
    property BorderIcons: TBorderIcons read FBorderIcons write SetBorderIcons default [];
    property Caption: TCaption read FCaption write FCaption;
    property Control: TControl read FControl write SetControl;
    property EnterKeyAction: TEnterKeyAction read FEnterKeyAction write FEnterKeyAction default enterKeyGotoNextControl;
    property FormContainer: TForm read FFormContainer;
    property Position: TPosition read FPosition write FPosition default poScreenCenter;
    property ShowHint: Boolean read FShowHint write FShowHint default false;
    {$IFDEF DELPHI2009_OR_ABOVE}
    property AlphaBlend: Boolean read FAlphaBlend write FAlphaBlend default false;
    property AlphaBlendValue: byte read FAlphaBlendValue write FAlphaBlendValue default 255;
    property TransparentColor: Boolean read FTransparentColor write FTransparentColor default false;
    property TransparentColorValue: TColor read FTransparentColorValue write FTransparentColorValue default clBlack;
    {$ENDIF}
    property WindowState: TWindowState read FWindowState write FWindowState default wsNormal;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    procedure CreateFormContainer;
    procedure SetContainerProperties; virtual;
    procedure SaveControlProperties;
    procedure RestoreControlProperties;
    procedure DoFormClose; virtual;
    procedure DoFormDeactivate; virtual;
    procedure DoFormKeyPress(var Key: Char); virtual;
    procedure DoFormResize; virtual;
    procedure DoFormShow; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Close; virtual;
    procedure ExecuteFromControl(aControl: TControl; posX, posY: integer);
    procedure ExecuteFromGrid(aGrid: TCustomGrid; Align: TAlignment);
    procedure Execute(ScreenCoord: TPoint); virtual;
  published
  end;

implementation

{TcyBaseContainer}

constructor TcyBaseContainer.Create(AOwner: TComponent);
begin
  inherited;
  FActive := false;
  FBorderStyle := bsNone;
  FBorderIcons := [];
  FPosition := poScreenCenter;
  FShowHint := false;
  FEnterKeyAction := enterKeyGotoNextControl;
  {$IFDEF DELPHI2009_OR_ABOVE}
  FAlphaBlend := false;
  FAlphaBlendValue := 255;
  FTransparentColor := false;
  FTransparentColorValue := clBlack;
  {$ENDIF}
  FWindowState := wsNormal;
end;

procedure TcyBaseContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if FControl <> nil
  then
    if (Operation = opRemove) and (AComponent = FControl)
    then FControl := nil;
end;

procedure TcyBaseContainer.SetControl(const Value: TControl);
begin
  if FActive then
    raise Exception.Create('Can'' t set control when Active!');

  FControl := Value;

  if Value <> nil
  then Value.FreeNotification(Self);  // Inform cyBaseContainer if component removed ...
end;

procedure TcyBaseContainer.SetBorderIcons(Value: TBorderIcons);
begin
  if FBorderIcons <> Value
  then FBorderIcons := Value;
end;

procedure TcyBaseContainer.SetBorderStyle(Value: TFormBorderStyle);
begin
  if FBorderStyle <> Value
  then FBorderStyle := Value;
end;

procedure TcyBaseContainer.SaveControlProperties;
begin
  FSaveParent := FControl.Parent;
  FSaveVisible := FControl.Visible;
  FSaveAlign := FControl.Align;
  FSaveWidth := FControl.Width;
  FSaveTop := FControl.Top;
  FSaveLeft := FControl.Left;
  FSaveHeight := FControl.Height;
end;

procedure TcyBaseContainer.RestoreControlProperties;
begin
  FControl.Visible := FSaveVisible;
  FControl.Align := FSaveAlign;
  FControl.Width := FSaveWidth;
  FControl.Height := FSaveHeight;
  FControl.Top := FSaveTop;
  FControl.Left := FSaveLeft;
  FControl.Parent := FSaveParent;
end;

procedure TcyBaseContainer.CreateFormContainer;
begin
  FFormContainer := TForm.Create(nil);
end;

procedure TcyBaseContainer.SetContainerProperties;
begin
  FFormContainer.KeyPreview := true;
  FFormContainer.AutoScroll := FBorderStyle in [bsSizeable, bsSizeToolWin];
  FFormContainer.BorderIcons := FBorderIcons;
  FFormContainer.BorderStyle := FBorderStyle;
  FFormContainer.Caption := Caption;
  FFormContainer.Position := FPosition;
  {$IFDEF DELPHI2009_OR_ABOVE}
  FFormContainer.AlphaBlend := FAlphaBlend;
  FFormContainer.AlphaBlendValue := FAlphaBlendValue;
  FFormContainer.TransparentColor := FTransparentColor;
  FFormContainer.TransparentColorValue := FTransparentColorValue;
  {$ENDIF}
  FFormContainer.Scaled := false;
  FFormContainer.ShowHint := FShowHint;
  FFormContainer.WindowState := FWindowState;
  FFormContainer.OnClose := OnFormClose;
  FFormContainer.OnDeactivate := OnFormDeactivate;
  FFormContainer.OnKeyPress := OnFormKeyPress;
  FFormContainer.OnResize := OnFormResize;
  FFormContainer.OnShow := OnFormShow;
end;

procedure TcyBaseContainer.ExecuteFromControl(aControl: TControl; posX, posY: integer);
var aPoint: TPoint;
begin
  aPoint := aControl.ClientToScreen(Point(posX, posY));
  Execute(aPoint);
end;

procedure TcyBaseContainer.ExecuteFromGrid(aGrid: TCustomGrid; Align: TAlignment);
var
  aRect: TRect;
  aPoint: TPoint;
  aCyGrid: TcyCustomGrid;
begin
  if FControl <> nil
  then
    try
      aCyGrid := TcyCustomGrid(aGrid);
      aRect := aCyGrid.CellRect(aCyGrid.Col, aCyGrid.Row);

      case Align of
        taRightJustify : aPoint := Point(aRect.Right - Control.Width, aRect.Bottom);
        taLeftJustify  : aPoint := Point(aRect.Left, aRect.Bottom);
        else             aPoint := Point( aRect.Left + ((aRect.Right-aRect.Left) div 2) - (FControl.Width div 2), aRect.Bottom )
      end;

      // Convert to screen coordinates :
      aPoint := aGrid.ClientToScreen(aPoint);

      // FControl Outside the screen?
      if aPoint.Y + FControl.Height > Screen.Height
      then aPoint.Y := aPoint.Y - FControl.Height;

      Execute(aPoint);
    except

    end;
end;

procedure TcyBaseContainer.Execute(ScreenCoord: TPoint);
begin
  //
end;

procedure TcyBaseContainer.OnFormShow(Sender: TObject);
begin
  DoFormShow;
end;

procedure TcyBaseContainer.DoFormShow;
begin
  FActive := true;

  if Assigned(FOnShow)
  then FOnShow(Self);
end;

procedure TcyBaseContainer.OnFormClose(Sender: TObject; var Action: TCloseAction);
begin
  DoFormClose;
end;

procedure TcyBaseContainer.DoFormClose;
begin
  RestoreControlProperties;
  FActive := false;

  if Assigned(FOnClose)
  then FOnClose(Self);
end;

procedure TcyBaseContainer.OnFormKeyPress(Sender: TObject; var Key: Char);
begin
  DoFormKeyPress(Key);
end;

procedure TcyBaseContainer.OnFormDeactivate(Sender: TObject);
begin
  DoFormDeactivate;
end;

procedure TcyBaseContainer.DoFormDeactivate;
begin
  if Assigned(FOnDeactivate)
  then FOnDeactivate(Self);
end;

procedure TcyBaseContainer.OnFormResize(Sender: TObject);
begin
  DoFormResize;
end;

procedure TcyBaseContainer.DoFormResize;
begin
  if Assigned(FOnResize)
  then FOnResize(Self);
end;

procedure TcyBaseContainer.Close;
begin
  //
end;

procedure TcyBaseContainer.DoFormKeyPress(var Key: Char);

      function NavigateToNextControl(onForm: TForm): Boolean;
      begin
        Result := false;
        if not onForm.Enabled then Exit;
        Result := true;

        // navigate if not Memos/RichEdits and Grids :
        if onForm.ActiveControl <> Nil then
          if (pos('memo', AnsiLowercase(onForm.ActiveControl.ClassName)) <> 0)
           or (pos('richedit', AnsiLowercase(onForm.ActiveControl.ClassName)) <> 0)
             or (pos('grid', AnsiLowercase(onForm.ActiveControl.ClassName)) <> 0)
          then Result := false;

        if Result then
          onForm.Perform(WM_NEXTDLGCTL, 0, 0);
      end;

begin
  if Assigned(FOnKeyPress)
  then FOnKeyPress(Self, Key);

  if Key = #13 then
    case FEnterKeyAction of
      // enterKeyDefault: ;

      enterKeyGotoNextControl:
      begin
        if NavigateToNextControl(FFormContainer) then
          Key := #0;
      end;

      enterKeyNothing:
        Key := #0;
    end;
end;

end.
