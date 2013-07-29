unit SWGeneralPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseGeneralPropertyPage, Vcl.StdCtrls;

type
  TfrmSWGeneralPropertyPage = class(TfrmBaseGeneralPropertyPage)
    grpSoftware: TGroupBox;
    btnBrowseWorkingDir: TButton;
    edtWorkingDir: TEdit;
    lbWorkingDir: TLabel;
    btnBrowseExe: TButton;
    lbInfo2: TLabel;
    edtParameters: TEdit;
    lbParameters: TLabel;
    edtPathExe: TEdit;
    lbPathExe: TLabel;
    procedure edtPathExeExit(Sender: TObject);
    procedure btnBrowseExeClick(Sender: TObject);
    procedure btnBrowseWorkingDirClick(Sender: TObject);
  private
    { Private declarations }
    procedure CheckPropertyPathExe(Edit: TEdit);
  strict protected
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmSWGeneralPropertyPage: TfrmSWGeneralPropertyPage;

implementation

uses
  AppConfig, PropertyItem, ulSysUtils, ulNodeDataTypes, ulFileFolder;

{$R *.dfm}

procedure TfrmSWGeneralPropertyPage.btnBrowseExeClick(Sender: TObject);
begin
  OpenDialog1.Filter     := 'Executables (*.exe)|*.exe|All files|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(edtPathExe.Text));
  if (OpenDialog1.Execute) then
  begin
    edtPathExe.Text := AbsoluteToRelative(OpenDialog1.FileName);
    CheckPropertyPathExe(edtPathExe);
  end;
end;

procedure TfrmSWGeneralPropertyPage.btnBrowseWorkingDirClick(Sender: TObject);
var
  PathTemp: string;
begin
  PathTemp := BrowseForFolder('', RelativeToAbsolute(edtWorkingDir.Text));
  if (PathTemp <> '') then
    edtWorkingDir.Text := AbsoluteToRelative(PathTemp);
end;

procedure TfrmSWGeneralPropertyPage.CheckPropertyPathExe(Edit: TEdit);
begin
  if not(FileFolderPageWebExists(Edit.Text)) then
  begin
    //File not found - Change font color with red
    Edit.Font.Color := clRed;
    Edit.Hint       := msgFileNotFound;
  end
  else begin
    //File found - Change font color with clWindowText
    Edit.Font.Color  := clWindowText;
    Edit.Hint        := '';
  end;
end;

procedure TfrmSWGeneralPropertyPage.edtPathExeExit(Sender: TObject);
begin
  if Sender is TEdit then
    CheckPropertyPathExe(TEdit(Sender));
end;

function TfrmSWGeneralPropertyPage.InternalLoadData: Boolean;
var
  FileNodeData: TvFileNodeData;
begin
  Result := inherited;
  lbInfo2.Caption := Format(lbInfo2.Caption, [SUITE_WORKING_PATH, SUITE_DRIVE]);
  if Assigned(CurrentNodeData) then
  begin
    FileNodeData := TvFileNodeData(CurrentNodeData);
    edtPathExe.Text    := FileNodeData.PathExe;
    CheckPropertyPathExe(edtPathExe);
    edtParameters.Text := FileNodeData.Parameters;
    edtWorkingDir.Text := FileNodeData.WorkingDir;
  end;
end;

function TfrmSWGeneralPropertyPage.InternalSaveData: Boolean;
var
  FileNodeData: TvFileNodeData;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    FileNodeData := TvFileNodeData(CurrentNodeData);
    if (edtPathExe.Text = '') then
      edtPathExe.Text := CONST_PATH_DRIVE;
    FileNodeData.PathExe    := edtPathExe.Text;
    FileNodeData.Parameters := edtParameters.Text;
    FileNodeData.WorkingDir := edtWorkingDir.Text;
  end;
end;

end.
