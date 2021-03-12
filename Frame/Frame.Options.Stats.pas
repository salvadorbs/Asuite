{
Copyright (C) 2006-2020 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Frame.Options.Stats;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, DefaultTranslator, Controls,
  Forms, Dialogs, Frame.BaseEntity, JPP.DoubleLineLabel, StdCtrls;

type

  { TfrmStatsOptionsPage }

  TfrmStatsOptionsPage = class(TfrmBaseEntityPage)
    gbASuite: TGroupBox;
    lbCat: TJppDoubleLineLabel;
    lblBuild: TJppDoubleLineLabel;
    lblProcessor: TJppDoubleLineLabel;
    lblRam: TJppDoubleLineLabel;
    lbOs: TJppDoubleLineLabel;
    lbSize: TJppDoubleLineLabel;
    lbSoftware: TJppDoubleLineLabel;
    gbSupport: TGroupBox;
    lbSpaceFree: TJppDoubleLineLabel;
    gbSystem: TGroupBox;
    lbSpaceUsed: TJppDoubleLineLabel;
    lbTotal: TJppDoubleLineLabel;
    lbUser: TJppDoubleLineLabel;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmStatsOptionsPage: TfrmStatsOptionsPage;

implementation

{$I ASuite.inc}

uses
  Utility.Misc, AppConfig.Main, Kernel.Types, VirtualTree.Methods,
  Kernel.ResourceStrings, BZSystem;

{$R *.lfm}

{ TfrmStatsOptionsPage }

function TfrmStatsOptionsPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('stats');
end;

function TfrmStatsOptionsPage.GetTitle: string;
begin
  Result := msgStats;
end;

function TfrmStatsOptionsPage.InternalLoadData: Boolean;
var
  Drive: String;
  ListStats: rListStats;
  intDiskSize, intDiskFree: Int64;
begin
  Result := inherited;

  //System
  lbOs.RightCaption := GetplatformAsString;
  lblBuild.RightCaption := GetPlatformInfo.Version;
  lblProcessor.RightCaption := BZCPUInfos.BrandName;
  lblRam.RightCaption   := FormatByteSize(GetMemoryStatus.TotalPhys);
  lbUser.RightCaption   := GetCurrentUserName;

  //Drive
  Drive := Config.Paths.SuiteDrive;

  if Length(Drive) > 0 then
  begin
    GetDiskFreeSpace(Drive, intDiskFree, intDiskSize);
    lbSize.RightCaption   := DiskFloatToString(intDiskSize, True);
    lbSpaceFree.RightCaption := DiskFloatToString(intDiskFree, True);
    lbSpaceUsed.RightCaption := DiskFloatToString((intDiskSize - intDiskFree), True);
  end;

  gbSupport.Caption := Format(gbSupport.Caption, [Drive]);

  //Launcher
  ListStats.SwCount  := 0;
  ListStats.CatCount := 0;
  Config.MainTree.IterateSubtree(nil, TVirtualTreeMethods.Create.UpdateListItemCount, @ListStats);
  lbSoftware.RightCaption := IntToStr(ListStats.SwCount);
  lbCat.RightCaption      := IntToStr(ListStats.CatCount);
  lbTotal.RightCaption    := IntToStr(ListStats.SwCount + ListStats.CatCount);
end;

end.
