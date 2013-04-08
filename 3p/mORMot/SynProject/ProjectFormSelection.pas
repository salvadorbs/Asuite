unit ProjectFormSelection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSelectionForm = class(TForm)
    List: TListBox;
    pnlTop: TPanel;
    edtFind: TEdit;
    btnNext: TButton;
    procedure ListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListData(Control: TWinControl; Index: Integer;
      var Data: String);
    procedure edtFindChange(Sender: TObject);
  public
    Lines: TStringList;
    Selected: integer;
  end;

var
  SelectionForm: TSelectionForm;


implementation

{$R *.dfm}

procedure TSelectionForm.ListDblClick(Sender: TObject);
begin
  Selected := List.ItemIndex;
  Close;
end;

procedure TSelectionForm.FormCreate(Sender: TObject);
begin
  Lines := TStringList.Create;
end;

procedure TSelectionForm.FormDestroy(Sender: TObject);
begin
  Lines.Free;
end;

procedure TSelectionForm.FormShow(Sender: TObject);
begin
  List.Count := Lines.Count;
  if cardinal(Selected)<cardinal(Lines.Count) then
    List.ItemIndex := Selected;
  edtFind.SetFocus;
  Selected := -1;
end;

procedure TSelectionForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_RETURN:
    ListDblClick(nil);
  VK_ESCAPE:
    Close;
  VK_F3:
    edtFindChange(nil);
  end;
end;

procedure TSelectionForm.ListData(Control: TWinControl; Index: Integer;
  var Data: String);
var i, sp: integer;
begin
  if cardinal(Index)<cardinal(Lines.Count) then begin
    Data := Lines[index];
    if (Data<>'') and (Data[1]<>'[') then begin
      while (Data<>'') and (Data[1] in ['0'..'9']) do delete(Data,1,1);
      sp := 5;
      for i := 1 to length(Data) do
        if Data[i]=' ' then
          inc(sp,2) else
          break;
      Data := StringOfChar(' ',sp)+Data;
    end;
  end;
end;

procedure TSelectionForm.edtFindChange(Sender: TObject);
var Search: string;
    i: integer;
begin
  edtFind.SetFocus;
  Search := UpperCase(edtFind.Text);
  if Search='' then
    exit;
  for i := List.ItemIndex+1 to Lines.Count-1 do
    if Pos(Search,UpperCase(Lines[i]))>0 then begin
      List.ItemIndex := i;
      exit;
    end;
  for i := 0 to List.ItemIndex-1 do
    if Pos(Search,UpperCase(Lines[i]))>0 then begin
      List.ItemIndex := i;
      exit;
    end;
end;

end.

