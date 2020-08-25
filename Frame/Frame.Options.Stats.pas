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
  TMemoryStatusEx = packed record
     dwLength: DWORD;
     dwMemoryLoad: DWORD;
     ullTotalPhys: Int64;
     ullAvailPhys: Int64;
     ullTotalPageFile: Int64;
     ullAvailPageFile: Int64;
     ullTotalVirtual: Int64;
     ullAvailVirtual: Int64;
     ullAvailExtendedVirtual: Int64;
   end;

  { TfrmStatsOptionsPage }

  TfrmStatsOptionsPage = class(TfrmBaseEntityPage)
    gbASuite: TGroupBox;
    lbCat: TJppDoubleLineLabel;
    lblBuild: TJppDoubleLineLabel;
    lblProcessor: TJppDoubleLineLabel;
    lblRam: TJppDoubleLineLabel;
    lbNamePc: TJppDoubleLineLabel;
    lbOs: TJppDoubleLineLabel;
    lbSize: TJppDoubleLineLabel;
    lbSoftware: TJppDoubleLineLabel;
    gbSupport: TGroupBox;
    lbSpaceFree: TJppDoubleLineLabel;
    gbSystem: TGroupBox;
    lbSpaceUsed: TJppDoubleLineLabel;
    lbTotal: TJppDoubleLineLabel;

    lbUser: TJppDoubleLineLabel;
    procedure lbSizeClick(Sender: TObject);
  private
    { Private declarations }
    function GetTotalPhysMemory: Int64;
    function BytesToGBStr(const Bytes: Int64; const DecimalPlaces: Byte;
                          const SeparateThousands: Boolean): string;
    function BytesToGB(const Bytes: Int64): Extended;
    function FloatToFixed(const Value: Extended; const DecimalPlaces: Byte;
                          const SeparateThousands: Boolean): string;
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

{$I ASuite.INC}

uses
  Utility.Misc, AppConfig.Main, Kernel.Types, VirtualTree.Methods, Windows,
  PJSysInfo, Kernel.ResourceStrings;

{$R *.lfm}

{ TfrmStatsOptionsPage }

function GlobalMemoryStatusEx(var lpBuffer : TMemoryStatusEx) : BOOL;  stdcall;
  external 'kernel32.dll' name 'GlobalMemoryStatusEx';

function TfrmStatsOptionsPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('stats');
end;

function TfrmStatsOptionsPage.GetTitle: string;
begin
  Result := msgStats;
end;

procedure TfrmStatsOptionsPage.lbSizeClick(Sender: TObject);
begin

end;

function TfrmStatsOptionsPage.GetTotalPhysMemory: Int64;
var
  MemoryEx: TMemoryStatusEx;
begin
  begin
    MemoryEx.dwLength := SizeOf(TMemoryStatusEx);
    GlobalMemoryStatusEx(MemoryEx);
    Result := MemoryEx.ullTotalPhys;
  end;
end;

function TfrmStatsOptionsPage.FloatToFixed(const Value: Extended; const DecimalPlaces: Byte;
  const SeparateThousands: Boolean): string;
const
  // float format specifiers
  cFmtSpec: array[Boolean] of Char = ('f', 'n');
begin
  Result := SysUtils.Format(
    '%.*' + cFmtSpec[SeparateThousands], [DecimalPlaces, Value]
  );
end;

function TfrmStatsOptionsPage.BytesToGB(const Bytes: Int64): Extended;
const
  cOneGB = 1024 * 1024 * 1024; // a gigabyte in bytes
begin
  Result := Bytes / cOneGB;
end;

function TfrmStatsOptionsPage.BytesToGBStr(const Bytes: Int64; const DecimalPlaces: Byte;
  const SeparateThousands: Boolean): string;
begin
  Result := FloatToFixed(BytesToGB(Bytes), DecimalPlaces, SeparateThousands);
end;

function TfrmStatsOptionsPage.InternalLoadData: Boolean;
var
  Drive: char;
  ListStats: rListStats;
begin
  Result := inherited;

  //System
  lbOs.RightCaption := TPJOSInfo.ProductName + ' ' + TPJOSInfo.Edition;
  lblBuild.RightCaption := Format('%d.%d.%d', [TPJOSInfo.MajorVersion, TPJOSInfo.MinorVersion, TPJOSInfo.BuildNumber]);
  lblProcessor.RightCaption := TPJComputerInfo.ProcessorName;
  lblRam.RightCaption   := BytesToGBStr(GetTotalPhysMemory, 2, True) + ' GB';
  lbNamePc.RightCaption := TPJComputerInfo.ComputerName;
  lbUser.RightCaption   := TPJComputerInfo.UserName;

  //Drive
  Drive := Config.Paths.SuiteDrive[1];
  gbSupport.Caption := Format(gbSupport.Caption, [Drive]);
  lbSize.RightCaption   := DiskSizeString(Drive, True);
  lbSpaceFree.RightCaption := DiskFreeString(Drive, True);
  lbSpaceUsed.RightCaption := DiskUsedString(Drive, True);

  //Launcher
  ListStats.SwCount  := 0;
  ListStats.CatCount := 0;
  Config.MainTree.IterateSubtree(nil, TVirtualTreeMethods.Create.UpdateListItemCount, @ListStats);
  lbSoftware.RightCaption := IntToStr(ListStats.SwCount);
  lbCat.RightCaption      := IntToStr(ListStats.CatCount);
  lbTotal.RightCaption    := IntToStr(ListStats.SwCount + ListStats.CatCount);
end;

end.
