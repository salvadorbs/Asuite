{

 TButtonedEdit control

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Andrea Mauri

}unit ButtonedEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Menus, ImgList, ExtCtrls;

type
  { TCustomEditButton }

  TCustomEditButton = class(TCustomSpeedButton)
  private
    { Private declarations }
    FDropDownMenu: TPopupMenu;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    procedure SetDropDownMenu(const AValue: TPopupMenu);
  protected
    { Protected declarations }
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property ImageIndex: Integer read FImageIndex;
    property Images: TCustomImageList read FImages;

    property OnClick;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

  end;

  { TEditButton }

  TEditButton = class(TCustomEditButton)
  public
    { Public declarations }
  published
    { Published declarations }
    property DropDownMenu;
    property Enabled;
    property Glyph;
    property Hint;
    property ImageIndex;
    property Visible;
  end;

  { TCustomButtonedEdit }

  TCustomButtonedEdit = class(TCustomControl)
  private
    { Private declarations }
    FAlignment: TAlignment;
    FCharCase: TEditCharCase;
    FColor: TColor;
    FFont: TFont;
    FImages: TCustomImageList;
    FLeftButton: TEditButton;
    FMaxLength: Integer;
    FOnEditTextChange: TNotifyEvent;
    FOnLeftButtonClick: TNotifyEvent;
    FOnRightButtonClick: TNotifyEvent;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FRightButton: TEditButton;
    FEditText: TEdit;
    FText: TCaption;
    function GetText: TCaption;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetCharCase(const AValue: TEditCharCase);
    procedure SetColor(const AValue: TColor);
    procedure SetFont(const AValue: TFont);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetMaxLength(const AValue: Integer);
    procedure SetPasswordChar(const AValue: Char);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetText(const AValue: TCaption);
  protected
    { Protected declarations }
    procedure DoLeftButtonClick(Sender: TObject); virtual;
    procedure DoRightButtonClick(Sender: TObject); virtual;
    procedure DoEditTextChange(Sender: TObject); virtual;

    property Alignment: TAlignment read FAlignment write SetAlignment;
    property CharCase: TEditCharCase read FCharCase write SetCharCase;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property Images: TCustomImageList read FImages write SetImages;
    property LeftButton: TEditButton read FLeftButton default nil;
    property RightButton: TEditButton read FRightButton default nil;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Text: TCaption read GetText write SetText;

    property OnChange: TNotifyEvent read FOnEditTextChange write FOnEditTextChange;
    property OnLeftButtonClick: TNotifyEvent read FOnLeftButtonClick write FOnLeftButtonClick;
    property OnRightButtonClick: TNotifyEvent read FOnRightButtonClick write FOnRightButtonClick;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TButtonedEdit }

  TButtonedEdit = class(TCustomButtonedEdit)
  public
    { Public declarations }
  published
    { Published declarations }
    property Align;
    property Alignment;
    property Anchors;
//    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property Images;
    property LeftButton;
    property ParentBiDiMode;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property RightButton;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnLeftButtonClick;
    property OnRightButtonClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc',[TButtonedEdit]);
end;

{ TEditButton }

procedure TCustomEditButton.SetDropDownMenu(const AValue: TPopupMenu);
begin
  if FDropDownMenu=AValue then exit;
  FDropDownMenu:=AValue;
end;

constructor TCustomEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetSubComponent(True);
end;

{ TCustomButtonedEdit }

procedure TCustomButtonedEdit.SetImages(const AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
end;

procedure TCustomButtonedEdit.SetMaxLength(const AValue: Integer);
begin
  if FMaxLength=AValue then exit;
  FMaxLength:=AValue;
  FEditText.MaxLength:= FMaxLength;
end;

procedure TCustomButtonedEdit.SetPasswordChar(const AValue: Char);
begin
  if FPasswordChar=AValue then exit;
  FPasswordChar:=AValue;
  FEditText.PasswordChar:= FPasswordChar;
end;

procedure TCustomButtonedEdit.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  FEditText.ReadOnly:= FReadOnly;
end;

function TCustomButtonedEdit.GetText: TCaption;
begin
  FText:= FEditText.Text;
  Result:= FText;
end;

procedure TCustomButtonedEdit.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  FEditText.Alignment:= FAlignment;
end;

procedure TCustomButtonedEdit.SetText(const AValue: TCaption);
begin
  if FText=AValue then exit;
  FEditText.Text:= AValue;
  FText:=AValue;
end;


procedure TCustomButtonedEdit.DoLeftButtonClick(Sender: TObject);
var
  X, Y: Integer;
  P1, P2: TPoint;
begin
  if not ReadOnly then
  begin
    if Assigned(FOnLeftButtonClick) then
      FOnLeftButtonClick(Self);

    if Assigned(FLeftButton.DropDownMenu) then
    begin
      P1.X:= 0;
      P1.Y:= Self.Height;
      P2:= Self.ClientToScreen(P1);
      X:= P2.x;
      Y:= P2.y;
      FLeftButton.DropDownMenu.PopUp(X, Y);
    end;
  end;
end;

procedure TCustomButtonedEdit.DoRightButtonClick(Sender: TObject);
var
  X, Y: Integer;
  P1, P2: TPoint;
begin
  if not ReadOnly then
  begin
    if Assigned(FOnRightButtonClick) then
      FOnRightButtonClick(Self);

    if Assigned(FRightButton.DropDownMenu) then
    begin
      P1.X:= FRightButton.Left;
      P1.Y:= Self.Height;
      P2:= Self.ClientToScreen(P1);
      X:= P2.x;
      Y:= P2.y;
      FRightButton.DropDownMenu.PopUp(X, Y);
    end;
  end;
end;

procedure TCustomButtonedEdit.DoEditTextChange(Sender: TObject);
begin
  if Assigned(FOnEditTextChange) then
    FOnEditTextChange(Self);
end;

procedure TCustomButtonedEdit.SetCharCase(const AValue: TEditCharCase);
begin
  if FCharCase=AValue then exit;
  FCharCase:=AValue;
  FEditText.CharCase:= FCharCase;
end;

procedure TCustomButtonedEdit.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  inherited SetColor(AValue);
  FColor:=AValue;
  FEditText.Color:= AValue;
end;

procedure TCustomButtonedEdit.SetFont(const AValue: TFont);
begin
  if FFont=AValue then exit;
  FFont:=AValue;
  FEditText.Font:= FFont;
end;

constructor TCustomButtonedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderStyle:= bsSingle;
  Height:= 26;
  Width:= 190;

  FEditText:= TEdit.Create(Self);
  with FEditText do
  begin
    Align:= alClient;
    BorderStyle:= bsNone;
    BorderSpacing.Left:= 3;
    BorderSpacing.Right:= 3;
    BorderSpacing.Top:= 3;
    Parent:= Self;
    ParentColor:= True;
    OnChange:= @DoEditTextChange;
  end;
  FFont:= FEditText.Font;

  FLeftButton := TEditButton.Create(Self);
  with FLeftButton do
  begin
    AutoSize:= True;
    Name:= 'LeftButton';
    Width := Self.Height;
    Height := Self.Height;
    Flat:= True;
    Align:= alLeft;
    Parent:= Self;
    OnClick := @DoLeftButtonClick;
  end;

  FRightButton := TEditButton.Create(Self);
  with FRightButton do
  begin
    AutoSize:= True;
    Name:= 'RightButton';
    Width := Self.Height;
    Height := Self.Height;
    Flat:= True;
    Align:= alRight;
    Parent:= Self;
    OnClick := @DoRightButtonClick;
  end;
  Color:= clWhite;
end;

destructor TCustomButtonedEdit.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$I tbuttonededit.lrs}

end.
