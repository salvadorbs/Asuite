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

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Controls, Forms, Icons.Application, Generics.Collections,
  Kernel.Consts, LCLIntf, LCLType, Icons.Base;

type
  TBaseIcons = class(TObjectList<TBaseIcon>);

  { TIconsManager }

  TIconsManager = class
  private
    { private declarations }
    FPathTheme: string;
    FItems: TBaseIcons;
    function GetPathTheme: string;

    procedure LoadAllIcons;
    procedure SetPathTheme(const Value: string);
    function FindByPath(APathFile: String): TApplicationIcon;
    function FindByName(AName: String): TBaseIcon;
  public
    { public declarations }
    constructor Create;
    destructor Destroy; override;

    procedure Clear(AOnlyStaticItems: Boolean);
    function GetIconIndex(AName: string): Integer;
    function GetPathIconIndex(APathIcon: string): Integer;
    procedure AddIcon(ABaseIcon: TBaseIcon);

    property PathTheme: string read GetPathTheme write SetPathTheme;
  end;

implementation

uses
  AppConfig.Main, Kernel.Logger, FileUtil, Utility.FileFolder;

{ TIconsManager }

constructor TIconsManager.Create;
begin
  FItems := TBaseIcons.Create(True);
end;

destructor TIconsManager.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TIconsManager.Clear(AOnlyStaticItems: Boolean);
var
  Item: TBaseIcon;
begin
  if AOnlyStaticItems then
  begin
    for Item in FItems do
      if Item.Static then
        FItems.Remove(Item);
  end
  else
    FItems.Clear;
end;

function TIconsManager.GetIconIndex(AName: string): Integer;
var
  Icon: TBaseIcon;
begin
  Result := -1;

  Icon := FindByName(AName);

  if Assigned(Icon) then
    Result := Icon.ImageIndex;
end;

function TIconsManager.GetPathIconIndex(APathIcon: string): Integer;
var
  Icon: TApplicationIcon;
begin
  Result := -1;

  //Before find the icon, if it isn't exists yet, I will load it
  Icon := FindByPath(APathIcon);

  if Assigned(Icon) then
    Result := Icon.ImageIndex
  else begin
    Icon := TApplicationIcon.Create(APathIcon);
    try
      Result := Icon.ImageIndex;
    finally
      AddIcon(Icon);
    end;
  end;
end;

procedure TIconsManager.AddIcon(ABaseIcon: TBaseIcon);
begin
  Assert(Assigned(ABaseIcon), 'ABaseIcon is not assigned!');

  try
    //It is doesn't necessary load now icon (so in this way, speed up icon loading)
    //Icon.LoadIcon;
  finally
    FItems.Add(ABaseIcon);
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
  Icon: TBaseIcon;
  sPath: string;
  IconFiles: TStringList;
begin
  TASuiteLogger.Enter('LoadAllIcons', Self);
  TASuiteLogger.Info('Search and load all icons in folder "%s"', [FPathTheme + ICONS_DIR]);

  Clear(True);
  //Load all icons in FPathTheme + ICONS_DIR
  if DirectoryExists(FPathTheme + ICONS_DIR) then
  begin
    IconFiles := FileUtil.FindAllFiles(FPathTheme + ICONS_DIR, '*' + EXT_ICO);
    try
      //Add new icon in FItems
      for sPath in IconFiles do
      begin
        Icon := FindByName(ExtractOnlyFileName(sPath));

        if not(Assigned(Icon)) then
          AddIcon(TApplicationIcon.Create(sPath, True));
      end;
    finally
      IconFiles.Free;
    end;
  end;
end;

procedure TIconsManager.SetPathTheme(const Value: string);
begin
  FPathTheme := value;
  LoadAllIcons;
end;

function TIconsManager.FindByPath(APathFile: String): TApplicationIcon;
var
  Item: TBaseIcon;
begin
  //Warning: only TApplicationIcon!
  Result := nil;

  for Item in FItems do
  begin
    if (Item is TApplicationIcon) and (TApplicationIcon(Item).PathFile = APathFile) then
    begin
      Result := TApplicationIcon(Item);
      break;
    end;
  end;
end;

function TIconsManager.FindByName(AName: String): TBaseIcon;
var
  Item: TBaseIcon;
begin
  Result := nil;

  for Item in FItems do
  begin
    //TODO: Doesn't work
    if (Item.Name = AName) then
    begin
      Result := Item;
      break;
    end;
  end;
end;

end.
