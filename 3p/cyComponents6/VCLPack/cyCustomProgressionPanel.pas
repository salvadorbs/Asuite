{   Component(s):
    TcyCustomProgressionPanel

    Description:
    Base and properties for Run-time progression panel components

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

unit cyCustomProgressionPanel;

interface

uses Classes, Windows, Forms, Messages, Controls, Graphics, StdCtrls, ExtCtrls,
      VCL.cyTypes, VCL.cyClasses, cyPanel, CySimpleGauge;

type
  TProgressionState = (psClosed, psOpened);

  TcyCustomProgressionPanel = class(TComponent)
  private
    FCanceled: Boolean;
    FParent: TWinControl;
    // *** Components to be created at run-time *** //
    FPanel: TcyPanel;
    FLabel: TLabel;
    FImage: TImage;
    FBevel: TBevel;
    FGauge: TCySimpleGauge;
    FBtnCancel: TButton;
    // ***  //
    FAutosize: boolean;
    FWidth: integer;
    FHeight: integer;
    FFlashWindow: boolean;
    FGlyph: TPicture;
    FFont: TFont;
    FDegrade: TcyGradient;
    FState: TProgressionState;
    FButtonCancel: Boolean;
    FGaugeVisible: Boolean;
    FGlyphAlign: TAlign;
    FBorderWidth: TBorderWidth;
    FCaption: String;
    FButtonCancelCaption: String;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FOnClose: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FGaugeMax: Double;
    FGaugeMin: Double;
    FOnCancelButtonClick: TNotifyEvent;
    procedure SetGlyph(const Value: TPicture);
    procedure SetFont(const Value: TFont);
    procedure SetAutosize(const Value: boolean);
    procedure SetDegrade(const Value: TcyGradient);
    procedure SetFlashWindow(const Value: boolean);
    procedure SetCaption(const Value: String);
    procedure SetButtonCancelCaption(const Value: String);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetBorderWidth(const Value: TBorderWidth);
    procedure SetButtonCancel(const Value: Boolean);
    procedure SetGaugeMax(const Value: Double);
    procedure SetGaugeMin(const Value: Double);
    procedure SetGaugeVisible(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
    procedure AdjustSize;
    procedure ButtonCancelClick(Sender: TObject);
    function ProcessMessageFromPanel(var Msg: TMsg): Boolean;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Autosize: boolean read FAutosize write SetAutosize default true;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 5;
    property ButtonCancelCaption: String read FButtonCancelCaption write SetButtonCancelCaption;
    property ButtonCancel: Boolean read FButtonCancel write SetButtonCancel default false;
    property Caption: String read FCaption write SetCaption;
    property Degrade: TcyGradient read FDegrade write SetDegrade;
    property FlashWindow: boolean read FFlashWindow write SetFlashWindow default true;
    property Font: TFont read FFont write SetFont;
    property Height: integer read FHeight write FHeight default 55;
    property Gauge: Boolean read FGaugeVisible write SetGaugeVisible default true;
    property GaugeMax: Double read FGaugeMax write SetGaugeMax;
    property GaugeMin: Double read FGaugeMin write SetGaugeMin;
    property Glyph: TPicture read FGlyph write SetGlyph;
    property GlyphAlign: TAlign read FGlyphAlign write FGlyphAlign default alLeft;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property State: TProgressionState read FState;
    property Width: integer read FWidth write FWidth default 400;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCancelButtonClick: TNotifyEvent read FOnCancelButtonClick write FOnCancelButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open(ParentControl: TWinControl); overload; virtual;
    procedure Open; overload;
    procedure Close; virtual;
    function Canceled: boolean;
    function GetPanel: TcyPanel;
    function GetImage: TImage;
    function GetGauge: TCySimpleGauge;
    function GetLabel: TLabel;
    function GetCancelButton: TButton;
    procedure ProcessMessagesFromPanel;
    procedure DropMessages(ExceptFromControl: TWinControl);
  published
  end;

const
  constBtnHeight = 25;
  constBtnWidth = 75;
  constLabelHeight = 30;
  constLabelWidth = 300;

implementation

{TcyCustomProgressionPanel}
constructor TcyCustomProgressionPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taCenter;
  FAutosize := true;
  FBorderWidth := 5;
  FButtonCancel := false;
  FButtonCancelCaption := 'Cancel';
  FCaption := '';
  FDegrade := TcyGradient.Create(self);
  FFlashWindow := true;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FHeight := 55;
  FGlyph := TPicture.Create;
  FGlyphAlign := alLeft;
  FLayout := tlCenter;
  FGaugeVisible := true;
  FGaugeMax := 100;
  FGaugeMin := 0;
  FWidth := 400;

  FCanceled := false;
  FState := psClosed;
end;

destructor TcyCustomProgressionPanel.Destroy;
begin
  if FState = psOpened
  then
    if not Application.Terminated
    then Close;

  FDegrade.Free;
  FFont.Free;
  FGlyph.Free;
  inherited Destroy;
end;

procedure TcyCustomProgressionPanel.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;

  if FState = psOpened
  then FLabel.Alignment := FAlignment;
end;

procedure TcyCustomProgressionPanel.SetAutosize(const Value: boolean);
begin
  FAutosize := Value;
  AdjustSize;
end;

procedure TcyCustomProgressionPanel.SetBorderWidth(const Value: TBorderWidth);
begin
  FBorderWidth := Value;
  AdjustSize;
end;

procedure TcyCustomProgressionPanel.SetButtonCancel(const Value: Boolean);
begin
  if FButtonCancel <> Value
  then begin
    FButtonCancel := Value;

    if FState = psOpened
    then
      if FButtonCancel
      then FGauge.Width := FGauge.Width - FBorderWidth - constBtnWidth
      else FGauge.Width := FGauge.Width + FBorderWidth + constBtnWidth;
  end;
end;

procedure TcyCustomProgressionPanel.SetCaption(const Value: String);
begin
  FCaption := Value;

  if FState = psOpened
  then FLabel.Caption := FCaption;
end;

procedure TcyCustomProgressionPanel.SetDegrade(const Value: TcyGradient);
begin
  FDegrade := Value;

  if FState = psOpened
  then FPanel.Degrade.Assign(Self.Degrade);
end;

procedure TcyCustomProgressionPanel.SetFlashWindow(const Value: boolean);
begin
  FFlashWindow := Value;
end;

procedure TcyCustomProgressionPanel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);  // FFont := Value doesn' t work with TComponent base !!!

  if FState = psOpened
  then FLabel.Font.Assign(FFont);
end;

procedure TcyCustomProgressionPanel.SetGaugeVisible(const Value: Boolean);
begin
  FGaugeVisible := Value;

  if FState = psOpened
  then FGauge.Visible := FGaugeVisible;
end;

procedure TcyCustomProgressionPanel.SetGaugeMax(const Value: Double);
begin
  FGaugeMax := Value;

  if FState = psOpened
  then FGauge.Max := FGaugeMax;
end;

procedure TcyCustomProgressionPanel.SetGaugeMin(const Value: Double);
begin
  FGaugeMin := Value;

  if FState = psOpened
  then FGauge.Min := FGaugeMin;
end;

procedure TcyCustomProgressionPanel.SetLayout(const Value: TTextLayout);
begin
  FLayout := Value;

  if FState = psOpened
  then FLabel.Layout := FLayout;
end;

procedure TcyCustomProgressionPanel.FontChanged(Sender: TObject);
begin
  if FState = psOpened
  then FLabel.Font.Assign(FFont);
end;

function TcyCustomProgressionPanel.Canceled: boolean;
begin
  RESULT := FCanceled;
end;

procedure TcyCustomProgressionPanel.SetGlyph(const Value: TPicture);
begin
  FGlyph.Assign(Value); // FGlyph := Value; doesn' t work with TComponent base !!!

  if FState = psOpened
  then begin
    FImage.Picture.Assign(FGlyph);
    AdjustSize;
  end;
end;

procedure TcyCustomProgressionPanel.SetButtonCancelCaption(const Value: String);
begin
  FButtonCancelCaption := Value;

  if FState = psOpened
  then FBtnCancel.Caption := FButtonCancelCaption;
end;

function  TcyCustomProgressionPanel.GetPanel: TcyPanel;
begin
  RESULT := FPanel;
end;

function  TcyCustomProgressionPanel.GetImage: TImage;
begin
  RESULT := FImage;
end;

function  TcyCustomProgressionPanel.GetGauge: TCySimpleGauge;
begin
  RESULT := FGauge;
end;

function  TcyCustomProgressionPanel.GetLabel: TLabel;
begin
  RESULT := FLabel;
end;

function  TcyCustomProgressionPanel.GetCancelButton: TButton;
begin
  RESULT := FBtnCancel;
end;

procedure TcyCustomProgressionPanel.Open(ParentControl: TWinControl);
begin
  if FState = psOpened then EXIT;
  FCanceled := false;
  FParent := ParentControl;
  if FFlashWindow then Windows.FlashWindow(FParent.Handle, true);

  // Create components :
  FPanel := TcyPanel.Create(Owner);
  with FPanel do
  begin
    Visible := false;
    Parent  := FParent;
    Ctl3D       := false;
    BorderStyle := bsSingle;
    BorderWidth := 0;
    Caption     := '';

    with Bevels.Add do
    begin
      Width := FBorderWidth;
      Style := bcTransparent;
    end;

    Degrade.Assign(Self.Degrade);
    BringToFront;
  end;

  FImage := TImage.Create(Owner);   
  with FImage do
  begin
    Parent := FPanel;
    Transparent := true;
    Align := FGlyphAlign;
    Center := true;

    if FGlyph.Graphic <> nil
    then Picture.Assign(FGlyph);
  end;

  FLabel := TLabel.Create(Owner);
  with FLabel do
  begin
    Parent := FPanel;
    Align := alClient;
    WordWrap := true;
    Alignment := FAlignment;
    Layout := FLayout;
    Transparent := true;
    Font.Assign(FFont);
    Caption := FCaption;    
  end;

  FBevel := TBevel.Create(Owner);
  with FBevel do
  begin
    Parent := FPanel;
    Align := alBottom;
    Shape := bsSpacer; 
  end;

  FBtnCancel := TButton.Create(Owner);
  with FBtnCancel do
  begin
    Parent := FPanel;
    Caption := FButtonCancelCaption;
    Visible := FButtonCancel;
  end;

  FGauge := TcySimpleGauge.Create(Owner);
  with FGauge do
  begin
    Parent := FPanel;
    Smooth := true;
    Position := 0;
    ReadOnly := true;
    Min := FGaugeMin;
    Max := FGaugeMax;
    Visible := FGaugeVisible;
  end;

  FState := psOpened;
  AdjustSize;
  FPanel.Visible := true;
  FParent.Update;

  if Assigned(FOnOpen)
  then FOnOpen(Self);
end;

procedure TcyCustomProgressionPanel.Open;
begin
  if not (Owner is TWinControl) then EXIT;
  Open(TWinControl(Owner));
end;

procedure TcyCustomProgressionPanel.ButtonCancelClick(Sender: TObject);
begin
  FCanceled := true;
  if Assigned(FOnCancelButtonClick)
  then FOnCancelButtonClick(Self);
end;

procedure TcyCustomProgressionPanel.AdjustSize;
var AutoHeight, AutoWidth: Integer;
begin
  if FState <> psOpened then EXIT;

  with FPanel do
  begin
    if FAutosize
    then begin
      AutoHeight := FBorderWidth * 3 + constBtnHeight;  // Top border + bottom border + separation with button ...

      if FImage.Picture.Graphic <> nil
      then begin
        inc(AutoHeight, FImage.Picture.Graphic.Height);

        if FGlyphAlign in [alTop, alBottom]
        then inc(AutoHeight, FBorderWidth + constLabelHeight);
      end
      else
        inc(AutoHeight, constLabelHeight);


      if AutoHeight > FParent.ClientHeight
      then Height := FParent.ClientHeight
      else Height := AutoHeight;

      AutoWidth := FBorderWidth * 2 + constLabelWidth; // Left border + right border + label width ...

      if FImage.Picture.Graphic <> nil
      then
        if not (FGlyphAlign in [alTop, alBottom])
        then inc(AutoWidth, FImage.Picture.Graphic.Width + FBorderWidth * 2);  // Glyph + separation wifth glyph ...

      if AutoWidth > FParent.ClientWidth
      then Width := FParent.ClientWidth
      else Width := AutoWidth;
    end
    else begin
      Height := FHeight;
      Width := FWidth;
    end;

    Left := (FParent.ClientWidth  div 2) - (Width  div 2);
    Top := (FParent.ClientHeight div 2) - (Height div 2);
  end;

  with FImage do
  begin
    if FImage.Picture.Graphic <> nil
    then begin
      Width := FImage.Picture.Graphic.Width + FBorderWidth * 2;
      Visible := true;
    end
    else
      Visible := false;
  end;

  with FBevel do
  begin
    Top := FPanel.Width; // Pass under FImage if FGlyphAlign = alBottom ...
    Height := constBtnHeight + FBorderWidth;
  end;

  with FBtnCancel do
  begin
    Left := FPanel.Width - FBorderWidth - constBtnWidth;
    Top := FPanel.Height - FBorderWidth - constBtnHeight;
    Height  := constBtnHeight;
    Width   := constBtnWidth;
    OnClick := ButtonCancelClick;
  end;

  with FGauge do
  begin
    Top := FBtnCancel.Top;
    Left := FBorderWidth;
    Height := 20;
    FGauge.Width := FPanel.Width - FBorderWidth * 2;

    if FButtonCancel
    then FGauge.Width := FGauge.Width - FBorderWidth - constBtnWidth;
  end;
end;

procedure TcyCustomProgressionPanel.Close;
begin
  if FState <> psOpened then EXIT;

  if Assigned(FOnClose)
  then FOnClose(Self);

  if FFlashWindow then Windows.FlashWindow(FParent.Handle, true);

  try
    FPanel.Visible := false;
    FGauge.Free;
    FBtnCancel.Free;
    FLabel.Free;
    FImage.Free;
    FBevel.Free;
    FPanel.Free;
  finally
    FState := psClosed;
  end;
end;

procedure TcyCustomProgressionPanel.ProcessMessagesFromPanel;
var
  Msg: TMsg;
  Cont: Boolean;
begin
  cont := ProcessMessageFromPanel(Msg);

  while Cont do
    cont := ProcessMessageFromPanel(Msg);
end;

function TcyCustomProgressionPanel.ProcessMessageFromPanel(var Msg: TMsg): Boolean;
var aHandle: HWND;                   
begin
  RESULT := false;
  if FCanceled then EXIT;
  aHandle := 0;    // aHandle := FPanel.Handle doesn' t work if you click outiside the panel before clicking panel's cancel button ...

  if PeekMessage(Msg, aHandle, 0, 0, PM_REMOVE)
  then begin
    Result := True;

    if Msg.hwnd = FBtnCancel.Handle
    then begin
      Windows.TranslateMessage(Msg);
      Windows.DispatchMessage(Msg);
    end
    else
      if Msg.message = WM_PAINT         // !!!         Only WM_PAINT message are not removed from queue          !!!
      then begin                        // In order to go outside  while Cont do, we need to dispatch the message //
        Windows.TranslateMessage(Msg);
        Windows.DispatchMessage(Msg);
      end;
  end;
end;

procedure TcyCustomProgressionPanel.DropMessages(ExceptFromControl: TWinControl);
var
  Msg: TMsg;
  ExceptionHWND: HWND;
  Cont: Boolean;

    function DropIt: Boolean;
    begin
      RESULT := false;

      if PeekMessage(Msg, 0, 0, 0, PM_REMOVE)   // PeekMessage allow to consult messages one by one ...
      then begin
        Result := True;

        if Msg.hwnd = ExceptionHWND
        then begin
          // process this one:
          Windows.TranslateMessage(Msg);
          Windows.DispatchMessage(Msg);
        end
        else
          if Msg.message = WM_PAINT  // We need to process WM_PAINT messages! If not, we can' t go outside the while ...
          then begin
            Windows.TranslateMessage(Msg);
            Windows.DispatchMessage(Msg);
          end;
      end;
    end;

begin
  if ExceptFromControl <> Nil
  then ExceptionHWND := ExceptFromControl.Handle
  else ExceptionHWND := 0;

  cont := DropIt;

  while Cont do
    cont := DropIt;
end;

end.
