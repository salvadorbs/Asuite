unit AdvancedPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseEntityPage, Vcl.ComCtrls,
  Vcl.StdCtrls;

type
  TfrmAdvancedPropertyPage = class(TfrmBaseEntityPage)
    cbShortcutDesktop: TCheckBox;
    grpScheduler: TGroupBox;
    cxScheduler: TComboBox;
    dtpSchDate: TDateTimePicker;
    dtpSchTime: TDateTimePicker;
    grpHotkey: TGroupBox;
    GroupBox1: TGroupBox;
    cbHideSoftware: TCheckBox;
    cbDontInsertMRU: TCheckBox;
    cbDontInsertMFU: TCheckBox;
    cbHotKey: TCheckBox;
    cxHotkey1: TComboBox;
    cxHotKey2: TComboBox;
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
  frmAdvancedPropertyPage: TfrmAdvancedPropertyPage;

implementation

uses
  AppConfig;

{$R *.dfm}

{ TfrmAdvancedPropertyPage }

function TfrmAdvancedPropertyPage.GetImageIndex: Integer;
begin
  Result := IMAGELARGE_INDEX_Advanced;
end;

function TfrmAdvancedPropertyPage.GetTitle: string;
begin
  Result := 'Advanced';
end;

function TfrmAdvancedPropertyPage.InternalLoadData: Boolean;
begin

end;

function TfrmAdvancedPropertyPage.InternalSaveData: Boolean;
begin

end;

end.
