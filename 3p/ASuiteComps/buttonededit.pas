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
  Windows, LCLProc, Graphics, LazLogger;

type
  TButtonPosition = (bpLeft, bpRight);

  { TCustomGlyphButton }

  TCustomGlyphButton = class(TPersistent)
  private
    FButton: TCustomSpeedButton;
    FDropDownMenu: TPopupMenu;
    FPosition: TButtonPosition;

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
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;  
    property Visible: Boolean read GetVisible write SetVisibile default False;

    //Events
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;

    constructor Create(AOwner: TComponent; APosition: TButtonPosition);

    procedure UpdateSize;
    procedure Invalidate;
    procedure Assign(ASource: TPersistent); override;
  end;

  { TGlyphButton }

  TGlyphButton = class(TCustomGlyphButton)
  published
    property Images;
    property ImageIndex;
    property Visible;
  end;

  { TCustomButtonedEdit }

  TCustomButtonedEdit = class(TCustomControl)
  private
    FFont: TFont;
    FLeftButton: TGlyphButton;
    FEditText: TEdit;
    FOnEditTextChange: TNotifyEvent;
    FRightButton: TGlyphButton;

    procedure DoEditTextChange(Sender: TObject);
    function GetOnLeftButtonClick: TNotifyEvent;
    function GetOnRightButtonClick: TNotifyEvent;
    procedure SetFont(AValue: TFont);
    procedure SetLeftButton(AValue: TGlyphButton);
    procedure SetOnLeftButtonClick(AValue: TNotifyEvent);
    procedure SetOnRightButtonClick(AValue: TNotifyEvent);
    procedure SetRightButton(AValue: TGlyphButton);

    procedure UpdateSize;
  protected     
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
                                       
    property Font: TFont read FFont write SetFont;
    property LeftButton: TGlyphButton read FLeftButton write SetLeftButton;
    property RightButton: TGlyphButton read FRightButton write SetRightButton;

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
//    property ReadOnly;
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

procedure TCustomGlyphButton.SetVisibile(AValue: Boolean);
begin
  if AValue <> (FButton.Visible) then
  begin
    FButton.Visible := AValue;
  end;
end;

procedure TCustomGlyphButton.SetImages(AValue: TCustomImageList);
begin
  if FButton.Images <> AValue then
  begin
    FButton.Images := AValue;
  end;
end;

procedure TCustomGlyphButton.SetOnClick(AValue: TNotifyEvent);
begin
  FButton.OnClick := AValue;
end;

function TCustomGlyphButton.GetVisible: Boolean;
begin
  Result := FButton.Visible;
end;

procedure TCustomGlyphButton.SetDropDownMenu(AValue: TPopupMenu);
begin
  if FDropDownMenu <> AValue then
    FDropDownMenu := AValue;
end;

function TCustomGlyphButton.GetImageIndex: TImageIndex;
begin
  Result := FButton.ImageIndex;
end;

function TCustomGlyphButton.GetImages: TCustomImageList;
begin
  Result := FButton.Images;
end;

function TCustomGlyphButton.GetOnClick: TNotifyEvent;
begin
  Result := FButton.OnClick;
end;

procedure TCustomGlyphButton.SetImageIndex(AValue: TImageIndex);
begin
  if FButton.ImageIndex <> AValue then
    FButton.ImageIndex := AValue;
end;

constructor TCustomGlyphButton.Create(AOwner: TComponent;
  APosition: TButtonPosition);
begin                       
  FButton := TCustomSpeedButton.Create(AOwner);

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
  FButton.Parent := TWinControl(AOwner);
  FButton.Visible := False;

  UpdateSize;
end;

procedure TCustomGlyphButton.UpdateSize;
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

procedure TCustomGlyphButton.Invalidate;
begin
  FButton.Invalidate;
end;

procedure TCustomGlyphButton.Assign(ASource: TPersistent);
begin
  if ASource is TCustomGlyphButton then
  begin
    Images := TCustomGlyphButton(ASource).Images;
    ImageIndex := TCustomGlyphButton(ASource).ImageIndex;
    Visible := TCustomGlyphButton(ASource).Visible;
  end;
end;

{ TCustomButtonedEdit }

function TCustomButtonedEdit.GetOnRightButtonClick: TNotifyEvent;
begin
  if Assigned(FRightButton) then
    Result := FRightButton.OnClick;
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

procedure TCustomButtonedEdit.SetLeftButton(AValue: TGlyphButton);
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

procedure TCustomButtonedEdit.SetRightButton(AValue: TGlyphButton);
begin
  FRightButton.Assign(AValue);
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

  //Left Button
  FLeftButton := TGlyphButton.Create(Self, bpLeft);
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

  //Right Button
  FRightButton := TGlyphButton.Create(Self, bpRight);

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

