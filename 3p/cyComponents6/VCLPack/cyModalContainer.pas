{   Component(s):
    tcyModalContainer

    Description:
    Create a form in popup style with any control inside until it loose focus.

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

unit cyModalContainer;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Types, Graphics, Forms, Controls, ExtCtrls, cyBaseContainer;

type
  TcyModalContainer = class(TcyBaseContainer)
  private
    FEscKeyAction: TModalResult;
    fModalResult: TModalResult;
  protected
    procedure DoFormClose; override;
    procedure DoFormKeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(ScreenCoord: TPoint); overload; override;  
    procedure Execute; overload;
    function Execute(aCaption: String; aControl: TControl): TModalResult; overload;
    procedure Close; override;
    property Active;
    property FormContainer;
    property ModalResult: TModalResult read fModalResult;
  published
    property BorderStyle default bsSingle;
    property BorderIcons default [biSystemMenu];
    property Caption;
    property Control;
    property ShowHint;
    {$IFDEF DELPHI2009_OR_ABOVE}
    property AlphaBlend;
    property AlphaBlendValue;
    property TransparentColor;
    property TransparentColorValue;
    {$ENDIF}
    property OnClose;
    property OnKeyPress;
    property OnShow;
    property OnResize;
    property EnterKeyAction;
    property EscKeyAction: TModalResult read FEscKeyAction write FEscKeyAction default mrNone;
  end;

function ShowModalControl(aControl: TControl; BS: TFormBorderStyle; BI: TBorderIcons; WS: TWindowState; aColor: TColor; BW: Integer; Title: String; BeforeShowModal: TNotifyEvent): TModalResult;
function ShowModalPanel(aPanel: TCustomPanel; Title: String; ShowCloseIcon: Boolean; BeforeShowModal: TNotifyEvent): TModalResult;

implementation

function ShowModalPanel(aPanel: TCustomPanel; Title: String; ShowCloseIcon: Boolean; BeforeShowModal: TNotifyEvent): TModalResult;
var bi: TBorderIcons;
begin
  if aPanel.ParentBackground then            // Because of XPManifest ...
  begin
    aPanel.ParentBackground := false;
    aPanel.ParentBackground := true;
  end;

  if aPanel is TPanel then
  begin
    TPanel(aPanel).BevelOuter := BvNone;
    TPanel(aPanel).BevelInner := BvNone;
  end;

  if ShowCloseIcon
  then bi := [BISystemMenu]
  else bi := [];

  Result := ShowModalControl(aPanel, BsSingle, bi, WsNormal, ClBtnFace, 0, Title, BeforeShowModal)
end;

function ShowModalControl(aControl: TControl; BS: TFormBorderStyle; BI: TBorderIcons; WS: TWindowState; aColor: TColor; BW: Integer; Title: String; BeforeShowModal: TNotifyEvent): TModalResult;
var ModalFrm  : TForm;
    Old_Parent   : TWinControl;
    Old_Visible  : Boolean;
    Old_X, Old_Y : Integer;
begin
  Application.CreateForm(TForm, ModalFrm);

  with ModalFrm do
  begin
    Caption := Title; BorderIcons := BI; BorderStyle := Bs; BorderWidth := BW; WindowState  := WS;
    Color := aColor; Scaled := false; KeyPreview := True; Position := PoScreenCenter;
    ClientHeight := aControl.Height; ClientWidth := aControl.Width;
  end;

  Old_Parent := aControl.Parent; Old_Visible  := aControl.Visible; Old_X := aControl.Left; Old_Y := aControl.Top;
  aControl.Parent  := ModalFrm; aControl.Top := 0; aControl.Left := 0; aControl.Visible := True;

  if Assigned(BeforeShowModal) then
    BeforeShowModal(ModalFrm);

  Result := ModalFrm.ShowModal;

  aControl.Visible := Old_Visible;
  aControl.Left := Old_X; aControl.Top := Old_Y;
  aControl.Parent  := Old_Parent;

  ModalFrm.Release;
end;

{TcyModalContainer}
constructor TcyModalContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsSingle;
  BorderIcons := [biSystemMenu];
  FEscKeyAction := mrNone;
  fModalResult := mrNone;
end;

procedure TcyModalContainer.DoFormClose;
begin
  inherited;
  FormContainer.Release;
end;

procedure TcyModalContainer.Execute(ScreenCoord: TPoint);
begin
  if (not Active) and (Control <> nil)
  then begin
    CreateFormContainer;
    SetContainerProperties;
    FormContainer.Position := poDesigned;
    FormContainer.Top := ScreenCoord.Y;
    FormContainer.Left := ScreenCoord.X;
    FormContainer.ClientWidth := Control.Width;
    FormContainer.ClientHeight := Control.Height;

    SaveControlProperties;
    Control.Parent := FormContainer;
    Control.Align := alClient;
    Control.Visible := true;
    fModalResult := FormContainer.ShowModal;
  end;
end;

procedure TcyModalContainer.Execute;
begin
  if (not Active) and (Control <> nil)
  then begin
    CreateFormContainer;
    SetContainerProperties;
    FormContainer.ClientWidth := Control.Width;
    FormContainer.ClientHeight := Control.Height;

    SaveControlProperties;
    Control.Parent := FormContainer;
    Control.Align := alClient;
    Control.Visible := true;
    fModalResult := FormContainer.ShowModal;
  end;
end;

function TcyModalContainer.Execute(aCaption: String; aControl: TControl): TModalResult;
begin
  Caption := aCaption;
  Control := aControl;
  Execute;
  Result := fModalResult;
end;

// Called by you :
procedure TcyModalContainer.Close;
begin
  if not Active then Exit;

  {$IFDEF DELPHI2009_OR_ABOVE}
  FormContainer.ModalResult := mrClose;
  {$ELSE}
  FormContainer.ModalResult := mrAbort;
  {$ENDIF}
end;

procedure TcyModalContainer.DoFormKeyPress(var Key: Char);
begin
  Inherited;   // Call FOnKeyPress

  if (Key = #27) and (FEscKeyAction <> mrNone) then
  begin
    Key := #0;
    FormContainer.ModalResult := FEscKeyAction;
  end;
end;

end.
