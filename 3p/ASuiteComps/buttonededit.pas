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

unit ButtonedEdit;

{$I ASuiteComps.inc}

interface

uses
  Classes, SysUtils, StdCtrls, BCImageButton, Buttons, Controls, ImgList, LCLIntf,
  Windows, LCLProc, Graphics, Menus;

type
  TButtonPosition = (bpLeft, bpRight);

  TCustomButtonedEdit = class;

  { TCustomGlyphButton }

  TCustomGlyphButton = class(TCustomSpeedButton)
  private
    FDropDownMenu: TPopupMenu;
  public
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;

    procedure Click; override;
  end;

  { TCustomGlyphButtonOptions }

  TCustomGlyphButtonOptions = class(TPersistent)
  private
    FButton: TCustomGlyphButton;
    FParentControl: TCustomButtonedEdit;

    function GetDropDownMenu: TPopupMenu;
    function GetImageIndex: TImageIndex;
    function GetImages: TCustomImageList;
    function GetOnClick: TNotifyEvent;
    function GetVisible: Boolean;
    procedure SetDropDownMenu(AValue: TPopupMenu);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetVisibile(AValue: Boolean);
  public                                       
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;  
    property Visible: Boolean read GetVisible write SetVisibile default False;

    //Events
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;

    constructor Create(AOwner: TCustomButtonedEdit; APosition: TButtonPosition);

    procedure UpdateSize;
    procedure Invalidate;
    procedure Assign(ASource: TPersistent); override;
  end;

  { TGlyphButtonOptions }

  TGlyphButtonOptions = class(TCustomGlyphButtonOptions)
  published
    property DropDownMenu;
    property Images;
    property ImageIndex;
    property Visible;
  end;

  { TCustomButtonedEdit }

  TCustomButtonedEdit = class(TCustomControl)
  private
    FFont: TFont;
    FLeftButton: TGlyphButtonOptions;
    FEditText: TEdit;
    FOnEditTextChange: TNotifyEvent;
    FReadOnly: Boolean;
    FRightButton: TGlyphButtonOptions;

    procedure DoEditTextChange(Sender: TObject);
    function GetOnLeftButtonClick: TNotifyEvent;
    function GetOnRightButtonClick: TNotifyEvent;
    function GetText: TCaption;
    procedure SetFont(AValue: TFont);
    procedure SetLeftButton(AValue: TGlyphButtonOptions);
    procedure SetOnLeftButtonClick(AValue: TNotifyEvent);
    procedure SetOnRightButtonClick(AValue: TNotifyEvent);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetRightButton(AValue: TGlyphButtonOptions);
    procedure SetText(AValue: TCaption);

    procedure UpdateSize;
  protected     
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
                                       
    property Font: TFont read FFont write SetFont;
    property LeftButton: TGlyphButtonOptions read FLeftButton write SetLeftButton;
    property RightButton: TGlyphButtonOptions read FRightButton write SetRightButton;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Text: TCaption read GetText write SetText;

    //Events
    property OnChange: TNotifyEvent read FOnEditTextChange write FOnEditTextChange;
    property OnLeftButtonClick: TNotifyEvent read GetOnLeftButtonClick write SetOnLeftButtonClick;
    property OnRightButtonClick: TNotifyEvent read GetOnRightButtonClick write SetOnRightButtonClick;
  published
  end;

  { TButtonedEdit }

  TButtonedEdit = class(TCustomButtonedEdit)      
  public
    { Public declarations }
  published
    { Published declarations }
    property Align;
//    property Alignment;
    property Anchors;
//    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsSingle;
//    property CharCase;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
//    property Images;
    property LeftButton;
    property ParentBiDiMode;
//    property PasswordChar;
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
  RegisterComponents('ASuite Components',[TButtonedEdit]);
end;

{ TCustomGlyphButton }

procedure TCustomGlyphButton.Click;
var
  X, Y: Integer;
  P1, P2: TPoint;
begin
  if Assigned(FDropDownMenu) then
  begin
    P1.X:= 0;
    P1.Y:= Self.Height;
    P2:= Self.ClientToScreen(P1);
    X:= P2.x;
    Y:= P2.y;
    FDropDownMenu.PopUp(X, Y);
  end;
end;

{ TCustomGlyphButtonOptions }

procedure TCustomGlyphButtonOptions.SetVisibile(AValue: Boolean);
begin
  if AValue <> (FButton.Visible) then
  begin
    FButton.Visible := AValue;
  end;
end;

procedure TCustomGlyphButtonOptions.SetImages(AValue: TCustomImageList);
begin
  if FButton.Images <> AValue then
  begin
    FButton.Images := AValue;
  end;
end;

procedure TCustomGlyphButtonOptions.SetOnClick(AValue: TNotifyEvent);
begin
  FButton.OnClick := AValue;
end;

function TCustomGlyphButtonOptions.GetVisible: Boolean;
begin
  Result := FButton.Visible;
end;

procedure TCustomGlyphButtonOptions.SetDropDownMenu(AValue: TPopupMenu);
begin
  if FButton.DropDownMenu <> AValue then
    FButton.DropDownMenu := AValue;
end;

function TCustomGlyphButtonOptions.GetImageIndex: TImageIndex;
begin
  Result := FButton.ImageIndex;
end;

function TCustomGlyphButtonOptions.GetDropDownMenu: TPopupMenu;
begin
  Result := FButton.DropDownMenu;
end;

function TCustomGlyphButtonOptions.GetImages: TCustomImageList;
begin
  Result := FButton.Images;
end;

function TCustomGlyphButtonOptions.GetOnClick: TNotifyEvent;
begin
  Result := FButton.OnClick;
end;

procedure TCustomGlyphButtonOptions.SetImageIndex(AValue: TImageIndex);
begin
  if FButton.ImageIndex <> AValue then
    FButton.ImageIndex := AValue;
end;

constructor TCustomGlyphButtonOptions.Create(AOwner: TCustomButtonedEdit;
  APosition: TButtonPosition);
begin                       
  FButton := TCustomGlyphButton.Create(AOwner);
  FParentControl := AOwner;

  case APosition of
    bpLeft:
    begin
      FButton.Align := alLeft;
      FButton.Name := 'LeftButton';
    end;
    bpRight:
    begin
      FButton.Align := alRight;
      FButton.Name:= 'RightButton';
    end;
  end;
  FButton.AutoSize := True;
  FButton.Flat := True;
  FButton.Parent := TWinControl(FParentControl);
  FButton.Visible := False;

  UpdateSize;
end;

procedure TCustomGlyphButtonOptions.UpdateSize;
begin
  if ImageIndex <> -1 then
  begin
    //TODO: See imagelist muliresolution
    if Images <> nil then
    begin
      FButton.Width  := Images.Width;
      FButton.Constraints.MaxHeight := Images.Height;
    end
    else
    begin
      FButton.Width := 0;
      FButton.Height := 0;
    end;
  end;
end;

procedure TCustomGlyphButtonOptions.Invalidate;
begin
  FButton.Invalidate;
end;

procedure TCustomGlyphButtonOptions.Assign(ASource: TPersistent);
begin
  if ASource is TCustomGlyphButtonOptions then
  begin
    Images := TCustomGlyphButtonOptions(ASource).Images;
    ImageIndex := TCustomGlyphButtonOptions(ASource).ImageIndex;
    Visible := TCustomGlyphButtonOptions(ASource).Visible;
  end;
end;

{ TCustomButtonedEdit }

function TCustomButtonedEdit.GetOnRightButtonClick: TNotifyEvent;
begin
  if Assigned(FRightButton) then
    Result := FRightButton.OnClick;
end;

function TCustomButtonedEdit.GetText: TCaption;
begin
  Result := FEditText.Text;
end;

procedure TCustomButtonedEdit.SetFont(AValue: TFont);
begin
  if FFont <> AValue then
  begin
    FFont := AValue;
    FEditText.Font := FFont;
  end;
end;

function TCustomButtonedEdit.GetOnLeftButtonClick: TNotifyEvent;
begin
  if Assigned(FLeftButton) then
    Result := FLeftButton.OnClick;
end;

procedure TCustomButtonedEdit.DoEditTextChange(Sender: TObject);
begin
  if Assigned(FOnEditTextChange) then
    FOnEditTextChange(Self);
end;

procedure TCustomButtonedEdit.SetLeftButton(AValue: TGlyphButtonOptions);
begin
  FLeftButton.Assign(AValue);
end;

procedure TCustomButtonedEdit.SetOnLeftButtonClick(AValue: TNotifyEvent);
begin
  if Assigned(FLeftButton) then
    FLeftButton.OnClick := AValue;
end;

procedure TCustomButtonedEdit.SetOnRightButtonClick(AValue: TNotifyEvent);
begin
  if Assigned(FRightButton) then
    FRightButton.OnClick := AValue;
end;

procedure TCustomButtonedEdit.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly <> AValue then
  begin
    FReadOnly := AValue;
    FEditText.ReadOnly := AValue;
  end;
end;

procedure TCustomButtonedEdit.SetRightButton(AValue: TGlyphButtonOptions);
begin
  FRightButton.Assign(AValue);
end;

procedure TCustomButtonedEdit.SetText(AValue: TCaption);
begin
  FEditText.Text := AValue;
end;

procedure TCustomButtonedEdit.UpdateSize;
begin
  FLeftButton.UpdateSize;
  FRightButton.UpdateSize;
end;

class function TCustomButtonedEdit.GetControlClassDefaultSize: TSize;
begin
  Result := inherited GetControlClassDefaultSize;
end;

constructor TCustomButtonedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);                
  ParentColor := False;

  BorderStyle := bsSingle;

  //Buttons
  FLeftButton := TGlyphButtonOptions.Create(Self, bpLeft);
  FRightButton := TGlyphButtonOptions.Create(Self, bpRight);

  //EditBox
  FEditText:= TEdit.Create(Self);
  with FEditText do
  begin
    Align := alClient;
    BorderStyle := bsNone;
    BorderSpacing.Left := 3;
    BorderSpacing.Right := 3;
    BorderSpacing.Top := 3;
    Parent := Self;
    ParentColor := True;
    OnChange := DoEditTextChange;
  end;
  FFont := FEditText.Font;

  updateSize;
end;

destructor TCustomButtonedEdit.Destroy;
begin
  FreeAndNil(FLeftButton);
  FreeAndNil(FEditText);
  FreeAndNil(FRightButton);

  inherited Destroy;
end;

end.

