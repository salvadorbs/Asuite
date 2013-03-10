unit Stats;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmStats = class(TForm)
    gbSystem: TGroupBox;
    lbOs: TLabel;
    lbNamePc: TLabel;
    lbUser: TLabel;
    gbSupport: TGroupBox;
    lbSize: TLabel;
    lbSpaceUsed: TLabel;
    lbSpaceFree: TLabel;
    gbASuite: TGroupBox;
    lbSoftware: TLabel;
    lbCat: TLabel;
    lbTotal: TLabel;
    lbOs2: TLabel;
    lbNamePc2: TLabel;
    lbUser2: TLabel;
    lbSoftwareGroup: TLabel;
    lbSpaceFree2: TLabel;
    lbSize2: TLabel;
    lbSpaceUsed2: TLabel;
    lbSoftware2: TLabel;
    lbSoftwareGroup2: TLabel;
    lbCat2: TLabel;
    lbTotal2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure TranslateForm(Lingua:string);
  public
    { Public declarations }
  end;

var
  frmStats: TfrmStats;

implementation

{$R *.dfm}

uses Main, CommonUtils;

procedure TfrmStats.TranslateForm(Lingua: string);
begin
  with frmMain.xmldTranslate.DocumentElement.ChildNodes['Form13'] do
  begin
    //Stats          
    Caption           := ChildNodes['Form13Caption'].Text;
    //System
    gbSystem.caption  := ChildNodes['GroupBoxSystem'].Text;
    lbOs.caption      := ChildNodes['LabelOperatingSystem'].Text;
    lbNamePc.caption  := ChildNodes['LabelPcName'].Text;
    lbUser.caption    := ChildNodes['LabelCurrentUser'].Text;
    //Drive
    lbSize.caption      := ChildNodes['LabelSize'].Text;
    lbSpaceFree.caption := ChildNodes['LabelFreeSpace'].Text;
    lbSpaceUsed.caption := ChildNodes['LabelUsedSpace'].Text;
    gbSupport.Caption   := ChildNodes['GroupBoxDrive'].Text;
    //Launcher
    lbSoftware.caption  := ChildNodes['LabelSwInserted'].Text;
    lbSoftwareGroup.caption := ChildNodes['LabelSwGroupInserted'].Text;
    lbCat.caption       := ChildNodes['LabelCatInserted'].Text;
    lbTotal.caption     := ChildNodes['LabelTotal'].Text;
    gbASuite.caption    := ChildNodes['GroupBoxASuite'].Text;
  end;
end;

procedure TfrmStats.FormCreate(Sender: TObject);
var
  Drive : Char;
begin
  TranslateForm(LauncherOptions.LangName);
  //System
  lbOs2.caption        := GetWindowsVersion;
  lbNamePc2.caption    := GetComputerName;
  lbUser2.caption      := GetCurrentUserName;
  //Drive
  Drive                := StringReplace(ExtractFileDrive(ApplicationPath),':','',[])[1];
  gbSupport.Caption    := Format(gbSupport.Caption,[Drive]);
  lbSize2.caption      := DiskSizeString(Drive,true);
  lbSpaceFree2.caption := DiskFreeString(Drive,true);
  lbSpaceUsed2.caption := DiskUsedString(Drive,true);
  //Launcher
  with ListStats do
  begin
    SwCount      := 0;
    SwGroupCount := 0;
    CatCount     := 0;
    frmMain.vstList.IterateSubtree(nil, IterateSubtreeProcs.UpdateListItemCount, nil, [], False);
    lbSoftware2.caption  := IntToStr(SwCount);
    lbSoftwareGroup2.caption := IntToStr(SwGroupCount);
    lbCat2.caption       := IntToStr(CatCount);
    lbTotal2.caption     := IntToStr(SwCount + SwGroupCount + CatCount);
  end;
end;

end.
