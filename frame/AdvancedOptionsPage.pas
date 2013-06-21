unit AdvancedOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TfrmAdvancedOptionsPage = class(TfrmBaseOptionsPage)
    gbRecents: TGroupBox;
    lbMaxMRU: TLabel;
    lbNumbMRU: TLabel;
    cbMRU: TCheckBox;
    tbMRU: TTrackBar;
    gbMFU: TGroupBox;
    lbMaxMFU: TLabel;
    lbNumbMFU: TLabel;
    cbMFU: TCheckBox;
    tbMFU: TTrackBar;
    gbBackup: TGroupBox;
    lbMaxBackup: TLabel;
    lbNumbBackup: TLabel;
    cbBackup: TCheckBox;
    tbBackup: TTrackBar;
    grpClearElements: TGroupBox;
    lbClearElements: TLabel;
    cbRecents: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    cbCache: TCheckBox;
    btnClear: TButton;
    gbOtherFunctions: TGroupBox;
    CheckBox3: TCheckBox;
    cbScheduler: TCheckBox;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmAdvancedOptionsPage: TfrmAdvancedOptionsPage;

implementation

{$R *.dfm}

{ TfrmAdvancedOptionsPage }

function TfrmAdvancedOptionsPage.GetTitle: string;
begin
  Result := 'Advanced';
end;

end.
