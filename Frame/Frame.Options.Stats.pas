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

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Frame.BaseEntity, StdCtrls, Registry;

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
    
    lblBuild2: TLabel;
    lblBuild: TLabel;
    lblProcessor: TLabel;
    lblProcessor2: TLabel;
    lblRam: TLabel;
    lblRam2: TLabel;
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
  TfrmStatsOptionsPageClass = class of TfrmStatsOptionsPage;

var
  frmStatsOptionsPage: TfrmStatsOptionsPage;

implementation

{$I ASuite.INC}

uses
  Utility.Misc, AppConfig.Main, Kernel.Types, VirtualTree.Methods, Windows, PJSysInfo;

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
  //Result := DKLangConstW('msgStats');
end;

function TfrmStatsOptionsPage.GetTotalPhysMemory: Int64;var
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
  GetTotalPhysMemory;
  Result := inherited;
  //System
  lbOs2.Caption := TPJOSInfo.ProductName + ' ' + TPJOSInfo.Edition;
  lblBuild2.Caption := Format('%d.%d.%d', [TPJOSInfo.MajorVersion, TPJOSInfo.MinorVersion, TPJOSInfo.BuildNumber]);
  lblProcessor2.Caption := TPJComputerInfo.ProcessorName;
  lblRam2.Caption   := BytesToGBStr(GetTotalPhysMemory, 2, True) + ' GB';
  lbNamePc2.Caption := TPJComputerInfo.ComputerName;
  lbUser2.Caption   := TPJComputerInfo.UserName;
  //Drive
  Drive := Config.Paths.SuiteDrive[1];
  gbSupport.Caption := Format(gbSupport.Caption, [Drive]);
  lbSize2.Caption   := DiskSizeString(Drive, True);
  lbSpaceFree2.Caption := DiskFreeString(Drive, True);
  lbSpaceUsed2.Caption := DiskUsedString(Drive, True);
  //Launcher
  ListStats.SwCount  := 0;
  ListStats.CatCount := 0;
  Config.MainTree.IterateSubtree(nil, TVirtualTreeMethods.Create.UpdateListItemCount, @ListStats);
  lbSoftware2.Caption := IntToStr(ListStats.SwCount);
  lbCat2.Caption      := IntToStr(ListStats.CatCount);
  lbTotal2.Caption    := IntToStr(ListStats.SwCount + ListStats.CatCount);
end;

end.
