{   Component(s):
    TcyCustomDBGrid

    Description:
    A custom DBGrid with
    - revealed herited properties
    - OnSelectCell event
    - mouse wheel handling for records navigation, selecting rows (with Shift key) , do nothing or do as original
    - 2 clicks for massive rows selection (with Shift key)
    - Both scrollbars disabling option
    - CheckBox for each record
    - Auto stretch one column
    - Title custom appearence
    etc ...


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

unit cyDBGrid;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Grids, Graphics, DBGrids, VCL.cyGraphics, cyBaseDBGrid;

// AfterScroll and AfterScrollPause features :
{$IFDEF DELPHI2009_OR_ABOVE}
{$DEFINE SCROLL_FEATURE}
{$ENDIF}

type
  TcyCustomDBGrid = class(TcyBaseDBGrid)
  private
  protected
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); override;
  public
    procedure DrawCheckBoxColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
  published
  end;

  TcyDBGrid = class(TcyCustomDBGrid)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    // Herited from TDBGrid (Can not be published to avoid error on load project when a cyDBGrid is hiding his indicator) :
    property Col;              // Current column
    property ColCount;         // Displayed columnss
    property LeftCol;          // First displayed column
    property InplaceEditor;
    // Display errors on change ... property FixedCols;        // number of fixed columns
    property MouseJob;
    property Row;              // Current row
    property RowCount;         // Displayed rows
    property RowHeights;       // Array with Height of each row
    property VisibleColCount;  // ReadOnly - Visible data columns (Not Fixed Columns)
    property VisibleRowCount;  // ReadOnly - Visible data rows (Not Fixed Rows)
    property TopRow;           // Index of the first data row
    property Selection;        // Selected cell coordinates ...
    // Herited from TcyBaseDBGrid :
    property CheckedList;
    property ColumnIndex;
    property SelectedVisibleIndex;
  published
    // Herited from TDBGrid :
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    // Herited from TcyBaseDBGrid :
    property AllowDeleteRecord;
    property AllowInsertRecord;
    property AllowAppendRecord;
    property CheckBoxes;
    property ClientColumn;
    property CustomLayoutOptions;
    property DefaultDrawingCheckBox;
    property dgColumnResizeMode;
    property EditorOptions;
    property MouseWheelMode;
    property HorzScrollBar;
    property VertScrollBar;
    {$IFDEF SCROLL_FEATURE}
    property ScrollPauseMSeconds;
    {$ENDIF}
    property OnCheckBoxClick;
    property OnDrawCheckBox;
    property OnEditorAcceptKey;
    property OnEditorSetFieldText;
    property OnShowEditor;
    property OnSelectCell;
    property OnBeforePaint;
    property OnAfterPaint;
    property OnBeforeDrawCell;
    property OnAfterDrawCell;
    {$IFDEF SCROLL_FEATURE}
    property OnColWidthsChanged;
    property OnAfterScroll;
    property OnAfterScrollPause;
    {$ENDIF}
    property OnScrollBarMouseDown;
    property OnScrollBarMouseUp;
    property OnResize;
  end;

  TDBGrid = class(tcyDBGrid);
  // Let this line in order to overload dbgrids.TDBGrid class component in your projects
  // with tcyDBGrid (declare "cyDBGrid" in the uses of your unit after "DBGrids" declaration) :
  // Your old DBGrids will have the same properties/events/fonctions in your source code
  // at run time but are not visualized in the object inspector at design time.
  // Comment this line if you don' t want to.

implementation

// Note: the text has been already displayed, this procedure originally exists just to call OnDrawColumnCell :
procedure TcyCustomDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  // Redraw cell if column with the CheckBox and redraw text at correct position:
  if CheckBoxes.Visible and DataLink.Active then
    if DataCol = GetCheckBoxColumnIndex then
      DrawCheckBoxColumnCell(Rect, DataCol, Column, State);

  inherited;  // Call FOnDrawColumnCell if assigned ...
end;

procedure TcyCustomDBGrid.DrawCheckBoxColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  RectText, RectCheckBox: TRect;
  CheckBoxState: Boolean;
  Index: Integer;
begin
  // Repaint Text at the correct position :
  if DefaultDrawing then
  begin
    // Draw data cell background to erase previous rendering:
    Canvas.FillRect(Rect);  // with current brush ...
    CalcCheckBoxColumnCellLayout(Rect, RectText, RectCheckBox);
    // Draw text:
    DefaultDrawColumnCell(RectText, DataCol, Column, State);
  end;

  // Paint CheckBox :
  CheckBoxState := CheckedList.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  DrawCheckBox(RectCheckBox, CheckBoxState);
end;

{ TcyDBGrid }

constructor TcyDBGrid.Create(AOwner: TComponent);
begin
  inherited;

  ContentFieldsRender.BooleanField := bfDefault;  // Avoid KeyPress/MouseDown changes value ...
  ContentFieldsRender.GraphicField := gfDefault;
  ContentFieldsRender.MemoField    := mfDefault;
end;

end.
