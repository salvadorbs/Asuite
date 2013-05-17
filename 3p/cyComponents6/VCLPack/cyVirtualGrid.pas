unit cyVirtualGrid;
{   Component(s):
    tcyVirtualGrid

    Description:
    Component that generate cell coordonates to create a virtual grid.

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

interface

uses Classes, ExtCtrls, Controls, Messages, Types, SysUtils;

type
  TSizeMode = (smManual, smAutoFixed, smAutoStretched);
  TProcGeneratedCell = procedure (Sender: TObject; Rect: TRect; CoordX, CoordY: Integer) of object;
  TProcRowSize = procedure (Sender: TObject; CoordY: Integer; var Size: Word) of object;
  TProcColumnSize = procedure (Sender: TObject; CoordX: Integer; var Size: Word) of object;

  TcyVirtualGrid = class(TComponent)
  private
    FGeneratedCells: Array of Array of TRect;
    FValidCells: Boolean;  // Allow to know if some properties were changed since last generation
    FCellWidth: Word;
    FCellHeight: Word;
    FCellWidthMode: TSizeMode;
    FCellHeightMode: TSizeMode;
    FFromCoordX: Integer;
    FToCoordX: Integer;
    FFromCoordY: Integer;
    FToCoordY: Integer;
    FOnColumnSize: TProcColumnSize;
    FOnRowSize: TProcRowSize;
    FOnGeneratedCell: TProcGeneratedCell;
    procedure SetFromCoordX(const Value: Integer);
    procedure SetToCoordX(const Value: Integer);
    procedure SetFromCoordY(const Value: Integer);
    procedure SetToCoordY(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure GenerateCells(fromRect: TRect);
    function ColumnWidth(CoordX: Integer): Integer;
    function RowHeight(CoordY: Integer): Integer;
    function GetCellRect(CoordX, CoordY: Integer): TRect;
    function GetCellCoord(X, Y: Integer; var CoordX, CoordY: Integer): Boolean;
    property ValidCells: Boolean read FValidCells;
  published
    property CellWidth: Word read FCellWidth write FCellWidth default 0;
    property CellHeight: Word read FCellHeight write FCellHeight default 0;

    property CellWidthMode: TSizeMode read FCellWidthMode write FCellWidthMode default smAutoFixed;
    property CellHeightMode: TSizeMode read FCellHeightMode write FCellHeightMode default smAutoFixed;

    property FromCoordX: Integer read FFromCoordX write SetFromCoordX default 0;
    property ToCoordX: Integer read FToCoordX write SetToCoordX;
    property FromCoordY: Integer read FFromCoordY write SetFromCoordY default 0;
    property ToCoordY: Integer read FToCoordY write SetToCoordY;

    property OnRowSize: TProcRowSize read FOnRowSize write FOnRowSize;
    property OnColumnSize: TProcColumnSize read FOnColumnSize write FOnColumnSize;
    property OnGeneratedCell: TProcGeneratedCell read FOnGeneratedCell write FOnGeneratedCell;
  end;

const
  MsgInvalidCells = 'Invalid generated cells!';

implementation

{ TcyVirtualGrid }

constructor TcyVirtualGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCellWidth      := 0;
  FCellHeight     := 0;
  FCellWidthMode  := smAutoFixed;
  FCellHeightMode := smAutoFixed;
  FFromCoordX     := 0;
  FToCoordX       := 1;
  FFromCoordY     := 0;
  FToCoordY       := 1;
  FValidCells     := false;
end;

procedure TcyVirtualGrid.SetFromCoordX(const Value: Integer);
begin
  FFromCoordX := Value;
  FValidCells := false;
end;

procedure TcyVirtualGrid.SetFromCoordY(const Value: Integer);
begin
  FFromCoordY := Value;
  FValidCells := false;
end;

procedure TcyVirtualGrid.SetToCoordX(const Value: Integer);
begin
  FToCoordX := Value;
  FValidCells := false;
end;

procedure TcyVirtualGrid.SetToCoordY(const Value: Integer);
begin
  FToCoordY := Value;
  FValidCells := false;
end;

procedure TcyVirtualGrid.GenerateCells(fromRect: TRect);
var
  x, y: Integer;
  _NbCellsX, _NbCellsY, _StretchWidth, _StretchHeight, Tmp: Word;
  _CellRect: TRect;
  _CellLeft, _CellTop: Integer;
  _CellWidth, _CellHeight: Word;
begin
  if FFromCoordX > FToCoordX then EXIT;
  if FFromCoordY > FToCoordY then EXIT;

  Setlength(FGeneratedCells, FToCoordX - FFromCoordX + 1, FToCoordY - FFromCoordY + 1);
  FValidCells := true;  // Allow using functions before ending GenerateCells ...

  // Horizontal rects :
  _NbCellsX  := FToCoordX - FFromCoordX + 1;

  if FCellWidthMode <> smManual
  then FCellWidth := (fromRect.Right-fromRect.Left) div _NbCellsX;

  if FCellWidthMode = smAutoStretched
  then _StretchWidth := (fromRect.Right-fromRect.Left) - (FCellWidth * _NbCellsX);

  // Vertical rects :
  _NbCellsY   := FToCoordY - FFromCoordY + 1;

  if FCellHeightMode <> smManual
  then FCellHeight := (fromRect.Bottom-fromRect.Top) div _NbCellsY;

  if FCellHeightMode = smAutoStretched
  then _StretchHeight := (fromRect.Bottom-fromRect.Top) - (FCellHeight * _NbCellsY);

  _CellTop := fromRect.Top;

  for y := fromCoordY to toCoordY do     // Generate rows ...
  begin
    _CellHeight := FCellHeight;

    if FCellHeightMode = smAutoStretched
    then
      if _StretchHeight > 0
      then begin
        Dec(_StretchHeight, 1);
        Inc(_CellHeight, 1);
      end;

    if Assigned(FOnRowSize)
    then begin
      Tmp := _CellHeight;
      FOnRowSize(Self, y, Tmp);

      if FCellHeightMode = smManual
      then _CellHeight := Tmp;
    end;

    _CellLeft := fromRect.Left;

    for x := fromCoordX to toCoordX do   // Generate columns ...
    begin
      if y = fromCoordY      // First time we enter in this for and column widths are not defined ...
      then begin
        _CellWidth := FCellWidth;

        if FCellWidthMode = smAutoStretched
        then
          if _StretchWidth > 0
          then begin
            Dec(_StretchWidth, 1);
            Inc(_CellWidth, 1);
          end;

        if Assigned(FOnColumnSize)
        then begin
          Tmp := _CellWidth;
          FOnColumnSize(Self, x, Tmp);

          if FCellWidthMode = smManual
          then _CellWidth := Tmp;
        end;
      end
      else
        _CellWidth := FGeneratedCells[X-fromCoordX, 0].Right - FGeneratedCells[x-fromCoordX, 0].Left;

      _CellRect := classes.Rect(_CellLeft, _CellTop, _CellLeft + _CellWidth, _CellTop + _CellHeight);
      FGeneratedCells[x-fromCoordX, y-fromCoordY] := _CellRect;

      if Assigned(FOnGeneratedCell)
      then FOnGeneratedCell(self, _CellRect, x, y);

      Inc(_CellLeft, _CellWidth);
    end;

    Inc(_CellTop, _CellHeight);
  end;
end;

function TcyVirtualGrid.ColumnWidth(CoordX: Integer): Integer;
begin
  if FValidCells
  then
    RESULT := FGeneratedCells[CoordX - FFromCoordX, 0].Right - FGeneratedCells[CoordX - FFromCoordX, 0].Left
  else
    raise Exception.Create(MsgInvalidCells);
end;

function TcyVirtualGrid.RowHeight(CoordY: Integer): Integer;
begin
  if FValidCells
  then
    RESULT := FGeneratedCells[0, CoordY - FFromCoordY].Bottom - FGeneratedCells[0, CoordY - FFromCoordY].Top
  else
    raise Exception.Create(MsgInvalidCells);
end;

function TcyVirtualGrid.GetCellRect(CoordX, CoordY: Integer): TRect;
begin
  if FValidCells
  then
    RESULT := FGeneratedCells[CoordX - FFromCoordX, CoordY - FFromCoordY]
  else
    raise Exception.Create(MsgInvalidCells);
end;

function TcyVirtualGrid.GetCellCoord(X, Y: Integer; var CoordX, CoordY: Integer): Boolean;
var
  i: Integer;
  LocateX, LocateY: Boolean;
begin
  if FValidCells
  then begin
    // Locate CoordY :
    i := 0;
    LocateY := true;
    CoordY := -1;

    while (LocateY) and (i <= FToCoordY - FFromCoordY) do
    begin
      if Y >= FGeneratedCells[0, i].Top
      then
        if Y <= FGeneratedCells[0, i].Bottom
        then begin
          CoordY := i + FFromCoordY;
          LocateY := false;
        end;

      inc(i, 1);
    end;

    // Locate CoordX :
    i := 0;
    LocateX := true;
    CoordX := -1;

    while (LocateX) and (i <= FToCoordX - FFromCoordX) do
    begin
      if X >= FGeneratedCells[i, 0].Left
      then
        if X <= FGeneratedCells[i, 0].Right
        then begin
          CoordX := i + FFromCoordX;
          LocateX := false;
        end;

      inc(i, 1);
    end;

    RESULT := (not LocateX) and (not LocateY);
  end
  else
    raise Exception.Create(MsgInvalidCells);
end;

end.
