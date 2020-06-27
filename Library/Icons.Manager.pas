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

unit Icons.Manager;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Controls, IniFiles, Forms, Icons.Application, {Generics.Collections,
  IOUtils,} Kernel.Consts, LCLIntf, LCLType, LMessages;

type
  TBaseIcons = class(TObjectDictionary<string, TApplicationIcon>);

  TIconsManager = class
  private
    { private declarations }
    FPathTheme: string;
    FItems: TBaseIcons;
    function GetPathTheme: string;

    procedure LoadAllIcons;
    procedure SetPathTheme(const Value: string);
  public
    { public declarations }
    constructor Create;
    destructor Destroy; override;

    function GetIconIndex(AName: string): Integer;
    function GetPathIconIndex(APathIcon: string): Integer;

    property PathTheme: string read GetPathTheme write SetPathTheme;
  end;

implementation

uses
  AppConfig.Main, Kernel.Logger;

{ TIconsManager }

constructor TIconsManager.Create;
begin
  FItems := TBaseIcons.Create([doOwnsValues]);
end;

destructor TIconsManager.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TIconsManager.GetIconIndex(AName: string): Integer;
var
  Icon: TApplicationIcon;
begin
  Result := -1;
  Icon := FItems.Items[AName];
  if Assigned(Icon) then
    Result := Icon.ImageIndex;
end;

function TIconsManager.GetPathIconIndex(APathIcon: string): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
begin
  Result := -1;
  Flags := SHGFI_SYSICONINDEX or SHGFI_ICON or SHGFI_USEFILEATTRIBUTES;
  //Get index
  if SHGetFileInfo(PChar(Config.Paths.RelativeToAbsolute(APathIcon)), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
  begin
    DestroyIcon(FileInfo.hIcon);
    Result := FileInfo.iIcon;
  end;
end;

function TIconsManager.GetPathTheme: string;
begin
  if FPathTheme <> '' then
    Result := FPathTheme
  else
    Result := Config.Paths.SuitePathCurrentTheme;
end;

procedure TIconsManager.LoadAllIcons;
var
  Icon: TApplicationIcon;
  sPath: string;
begin
  TASuiteLogger.Enter('LoadAllIcons', Self);
  TASuiteLogger.Info('Search and load all icons in folder "%s"', [FPathTheme + ICONS_DIR]);

  FItems.Clear;
  //Load all icons in FPathTheme + ICONS_DIR
  if TDirectory.Exists(FPathTheme + ICONS_DIR) then
  begin
    for sPath in TDirectory.GetFiles(FPathTheme + ICONS_DIR, '*' + EXT_ICO) do
    begin
      //Create TBaseIcon, load icon and add it in FItems
      Icon := TApplicationIcon.Create(sPath);
      try
//        Icon.Load;
      finally
        FItems.Add(Icon.Name, Icon);
      end;
    end;
  end;
end;

procedure TIconsManager.SetPathTheme(const Value: string);
begin
  FPathTheme := value;
  LoadAllIcons;
end;

end.
