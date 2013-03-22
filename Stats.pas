unit Stats;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ASuiteForm;

type

  { TfrmStats }
  TfrmStats = class(TASuiteForm)
    gbASuite: TGroupBox;
    gbSupport: TGroupBox;
    gbSystem: TGroupBox;
    lbCat: TLabel;
    lbCat2: TLabel;
    lbNamePc: TLabel;
    lbNamePc2: TLabel;
    lbOs: TLabel;
    lbOs2: TLabel;
    lbSize: TLabel;
    lbSize2: TLabel;
    lbSoftware: TLabel;
    lbSoftware2: TLabel;
    lbSoftwareGroup: TLabel;
    lbSoftwareGroup2: TLabel;
    lbSpaceFree: TLabel;
    lbSpaceFree2: TLabel;
    lbSpaceUsed: TLabel;
    lbSpaceUsed2: TLabel;
    lbTotal: TLabel;
    lbTotal2: TLabel;
    lbUser: TLabel;
    lbUser2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStats: TfrmStats;

implementation

{$R *.dfm}

uses Main, ulCommonUtils, ulTreeView;

procedure TfrmStats.FormCreate(Sender: TObject);
var
  Drive: char;
begin
  //System
  lbOs2.Caption := GetWindowsVersion;
  lbNamePc2.Caption := GetComputerName;
  lbUser2.Caption := GetCurrentUserName;
  //Drive
  Drive := StringReplace(ExtractFileDrive(ParamStr(0)), ':', '', [])[1];
  gbSupport.Caption := Format(gbSupport.Caption, [Drive]);
  lbSize2.Caption := DiskSizeString(Drive, True);
  lbSpaceFree2.Caption := DiskFreeString(Drive, True);
  lbSpaceUsed2.Caption := DiskUsedString(Drive, True);
  //Launcher
  with ListStats do
  begin
    SwCount := 0;
    SwGroupCount := 0;
    CatCount := 0;
    frmMain.vstList.IterateSubtree(nil, IterateSubtreeProcs.UpdateListItemCount, nil, [], False);
    lbSoftware2.Caption := IntToStr(SwCount);
    lbSoftwareGroup2.Caption := IntToStr(SwGroupCount);
    lbCat2.Caption := IntToStr(CatCount);
    lbTotal2.Caption := IntToStr(SwCount + SwGroupCount + CatCount);
  end;
end;

end.
