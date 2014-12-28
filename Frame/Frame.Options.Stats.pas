unit Frame.Options.Stats;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseEntityPage, Vcl.StdCtrls, DKLang,
  Frame.BaseEntity;

type
  TfrmStatsOptionsPage = class(TfrmBaseEntityPage)
    gbASuite: TGroupBox;
    lbSoftware: TLabel;
    lbCat: TLabel;
    lbTotal: TLabel;
    lbSoftware2: TLabel;
    lbCat2: TLabel;
    lbTotal2: TLabel;
    gbSupport: TGroupBox;
    lbSize: TLabel;
    lbSpaceUsed: TLabel;
    lbSpaceFree: TLabel;
    lbSpaceFree2: TLabel;
    lbSize2: TLabel;
    lbSpaceUsed2: TLabel;
    gbSystem: TGroupBox;
    lbOs: TLabel;
    lbNamePc: TLabel;
    lbUser: TLabel;
    lbOs2: TLabel;
    lbUser2: TLabel;
    lbNamePc2: TLabel;
    DKLanguageController1: TDKLanguageController;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
  public
    { Public declarations }
  end;
  TfrmStatsOptionsPageClass = class of TfrmStatsOptionsPage;

var
  frmStatsOptionsPage: TfrmStatsOptionsPage;

implementation

uses
  Main, ulCommonUtils, ulTreeView, AppConfig;

{$R *.dfm}

{ TfrmStatsOptionsPage }

function TfrmStatsOptionsPage.GetImageIndex: Integer;
begin
  Result := IMAGELARGE_INDEX_Stats;
end;

function TfrmStatsOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgStats');
end;

function TfrmStatsOptionsPage.InternalLoadData: Boolean;
var
  Drive: char;
begin
  Result := inherited;
  //System
  lbOs2.Caption := TOSVersion.Name + IfThen(TOSVersion.ServicePackMajor > 0, Format(' Service Pack (%d.%d)',
                                           [TOSVersion.ServicePackMajor, TOSVersion.ServicePackMinor]), '');
  lbNamePc2.Caption := GetEnvironmentVariable('ComputerName'); //GetComputerName;
  lbUser2.Caption   := GetCurrentUserName;
  //Drive
  Drive := StringReplace(ExtractFileDrive(ParamStr(0)), ':', '', [])[1];
  gbSupport.Caption := Format(gbSupport.Caption, [Drive]);
  lbSize2.Caption   := DiskSizeString(Drive, True);
  lbSpaceFree2.Caption := DiskFreeString(Drive, True);
  lbSpaceUsed2.Caption := DiskUsedString(Drive, True);
  //Launcher
  with ListStats do
  begin
    SwCount  := 0;
    CatCount := 0;
    frmMain.vstList.IterateSubtree(nil, IterateSubtreeProcs.UpdateListItemCount, nil);
    lbSoftware2.Caption := IntToStr(SwCount);
    lbCat2.Caption      := IntToStr(CatCount);
    lbTotal2.Caption    := IntToStr(SwCount + CatCount);
  end;
end;

end.
