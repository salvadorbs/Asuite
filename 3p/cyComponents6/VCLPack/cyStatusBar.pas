{   Component(s):
    tcyBStatusBar

    Description:
    StatusBar that accept controls at design time.

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

unit cyStatusBar;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Windows, Classes, ComCtrls, CommCtrl, Controls, Graphics, Messages;

type
  TcyStatusBar = class(TStatusBar)
  private
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
  protected
    procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPanelRect(PanelIndex: Integer): TRect; overload;
    function GetPanelRect(Panel: TStatusPanel): TRect; overload;
    procedure PutControlOntoPanel(PanelIndex: Integer; aControl: TControl; x, y: Integer); overload;
    procedure PutControlOntoPanel(TargetPanel: TStatusPanel; aControl: TControl; x, y: Integer); overload;
  published
  end;

implementation

{ TcyStatusBar }
constructor TcyStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

function TcyStatusBar.GetPanelRect(PanelIndex: Integer): TRect;
begin
  // Not working on DELPHI 2009: Perform(SB_GETRECT, PanelIndex, integer( @R ));
  SendMessage(Handle, SB_GETRECT, PanelIndex, integer(@Result));
end;

procedure TcyStatusBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect);
begin
  inherited;
end;

function TcyStatusBar.GetPanelRect(Panel: TStatusPanel): TRect;
begin
  Result := GetPanelRect(Panel.Index);
end;

procedure TcyStatusBar.PutControlOntoPanel(PanelIndex: Integer; aControl: TControl; x, y: Integer);
var R: TRect;
begin
   // Panel rect :
   R := GetPanelRect(PanelIndex);

  aControl.Parent := Self;
  aControl.Top := R.Top + y;
  aControl.Left := R.Left + x;
end;

procedure TcyStatusBar.PutControlOntoPanel(TargetPanel: TStatusPanel; aControl: TControl; x, y: Integer);
begin
  PutControlOntoPanel(TargetPanel.Index, aControl, x, y);
end;

procedure TcyStatusBar.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  if Color = clBtnFace then
  begin
    Inherited;
    Exit;
  end;

  // TWinControl WMEraseBkGnd handling :

  { Only erase background if we're not doublebuffering or painting to memory. }
  if not DoubleBuffered or
  {$IF DEFINED(CLR)}
    (Message.OriginalMessage.WParam = Message.OriginalMessage.LParam) then
  {$ELSE}
    (TMessage(Message).wParam = TMessage(Message).lParam) then
  {$IFEND}
    begin
      FillRect(Message.DC, ClientRect, Brush.Handle);
    end;
end;

procedure TcyStatusBar.WMSize(var Message: TWMSize);
begin
  Realign;  // Make controls appear correctly (respect anchors and Align)
  Inherited;
end;

end.
