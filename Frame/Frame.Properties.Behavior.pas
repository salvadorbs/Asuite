unit Frame.Properties.Behavior;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DKLang, Frame.Properties.Base;

type
  TfrmBehaviorPropertyPage = class(TfrmBasePropertyPage)
    grpAutoExecute: TGroupBox;
    grpWindowState: TGroupBox;
    grpOnExecute: TGroupBox;
    cxAutoExecute: TComboBox;
    btnChangeOrder: TButton;
    cxActionOnExe: TComboBox;
    cxWindowState: TComboBox;
    DKLanguageController1: TDKLanguageController;
    procedure cxAutoExecuteChange(Sender: TObject);
    procedure btnChangeOrderClick(Sender: TObject);
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmBehaviorPropertyPage: TfrmBehaviorPropertyPage;

implementation

uses
  Kernel.Consts, Forms.PropertyItem, Kernel.Enumerations, Forms.Options, Frame.Options.Items;

{$R *.dfm}

{ TfrmMenuPropertyPage }

procedure TfrmBehaviorPropertyPage.btnChangeOrderClick(Sender: TObject);
begin
  if Assigned(CurrentNodeData) then
  begin
    CurrentNodeData.Autorun := TAutorunType(cxAutoExecute.ItemIndex);
    try
      Application.CreateForm(TfrmOptions, frmOptions);
      frmOptions.FormStyle := frmPropertyItem.FormStyle;
      frmOptions.Execute(TfrmItemsOptionsPage);
    finally
      frmOptions.Free;
    end;
  end;
end;

procedure TfrmBehaviorPropertyPage.cxAutoExecuteChange(Sender: TObject);
begin
  btnChangeOrder.Enabled := (cxAutoExecute.ItemIndex <> 0);
end;

function TfrmBehaviorPropertyPage.GetImageIndex: Integer;
begin
  Result := IMAGELARGE_INDEX_Behavior;
end;

function TfrmBehaviorPropertyPage.GetTitle: string;
begin
  Result := DKLangConstW('msgBehavior');
end;

function TfrmBehaviorPropertyPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    //Insert cat specific setting
    if CurrentNodeData.DataType = vtdtCategory then
      cxWindowState.Items.Insert(0, DKLangConstW('msgDefaultItemSettings'));
    cxActionOnExe.ItemIndex := Ord(CurrentNodeData.ActionOnExe);
    cxAutoExecute.ItemIndex := Ord(CurrentNodeData.Autorun);
    btnChangeOrder.Enabled  := (cxAutoExecute.ItemIndex <> 0);
    cxWindowState.ItemIndex := CurrentNodeData.WindowState
  end;
end;

function TfrmBehaviorPropertyPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    CurrentNodeData.ActionOnExe := TActionOnExecute(cxActionOnExe.ItemIndex);
    CurrentNodeData.Autorun     := TAutorunType(cxAutoExecute.ItemIndex);
    CurrentNodeData.WindowState := cxWindowState.ItemIndex;
  end;
end;

end.
