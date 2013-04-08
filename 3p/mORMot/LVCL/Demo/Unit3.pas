unit  Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Button1: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    Memo1: TMemo;
    Button2: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    Button6: TButton;
    GroupBox1: TGroupBox;
    Button7: TButton;
    Button8: TButton;
    RadioButton2: TRadioButton;
    RadioButton1: TRadioButton;
    Button9: TButton;
    Button10: TButton;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox2: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Edit1DblClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$R VistaAdm.res}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ComboBox1.Items.Add(Edit1.Text);
  ListBox1.Items.Add(Edit1.Text);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  ListBox1.Items.Clear;
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
  Memo1.AddLine('ComboBox1Click'+IntToStr(ComboBox1.ItemIndex));
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  Memo1.AddLine('ListBox1Click '+IntToStr(ListBox1.ItemIndex));
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Memo1.AddLine('ComboBox1Change '+IntToStr(ComboBox1.ItemIndex));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Edit1.Text:= ComboBox1.Text;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ComboBox1.Text:= Edit1.Text;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  CheckBox1.State:= cbGrayed;
  CheckBox2.State:= cbGrayed;
  CheckBox3.State:= cbGrayed;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
var i: Integer; prnt: TWinControl;
begin
  IF Sender is TRadioButton then
  begin
    prnt:= TWinControl(Sender).Parent;
    TRadioButton(Sender).State:= cbChecked;
    for i:= 0 to prnt.Controls.Count-1 do
      if (TComponent(prnt.Controls[i]).ClassType=TRadioButton) then
        TRadioButton(prnt.Controls[i]).Checked := prnt.Controls[i]=Sender;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  ComboBox1.DroppedDown:= not ComboBox1.DroppedDown;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  Memo1.AddLine('Button9Click');
  GroupBox1.Visible:= not GroupBox1.Visible;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Memo1.AddLineFmt('FormMouseDown %d %d', [X, Y]);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Memo1.AddLineFmt('FormMouseUp %d %d', [X, Y]);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Memo1.AddLineFmt('FormKeyDown %d', [Key]);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Memo1.AddLineFmt('FormKeyUp %d', [Key]);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Memo1.AddLineFmt('FormKeyPress #%d ''%s''', [Ord(Key), Key]);
end;

procedure TForm1.Edit1DblClick(Sender: TObject);
begin
  Memo1.AddLine('Edit1DblClick');
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  Memo1.AddLine('ListBox1DblClick');
end;

end.
