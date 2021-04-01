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
  SysUtils, Classes, Controls, Forms, Icons.Files, Generics.Collections,
  Kernel.Consts, LCLIntf, Icons.Base, Icons.ExtFile;

type
  TBaseIcons = class(TObjectList<TBaseIcon>);

  { TIconsManager }

  TIconsManager = class
  private
    { private declarations }
    FPathTheme: string;
    FItems: TBaseIcons;
    FExtItems: TBaseIcons;
    function GetPathTheme: string;

    procedure LoadAllIcons;
    procedure SetPathTheme(const Value: string);
    function FindByPath(APathFile: String): TFileIcon;
    function FindByName(AName: String): TBaseIcon;
    function FindByExt(AExtension: String): TExtFileIcon;
  public
    { public declarations }
    constructor Create;
    destructor Destroy; override;

    procedure Clear(AOnlyStaticItems: Boolean);
    function GetIconIndex(AName: string): Integer;
    function GetPathIconIndex(APathIcon: string): Integer;
    function GetExtIconIndex(AExtension: string): Integer;
    procedure AddIcon(ABaseIcon: TBaseIcon);
    procedure AddExtIcon(ABaseIcon: TBaseIcon);

    property PathTheme: string read GetPathTheme write SetPathTheme;
  end;

implementation

uses
  Kernel.Logger, FileUtil, LazFileUtils, Kernel.Instance, Kernel.Manager;

{ TIconsManager }

constructor TIconsManager.Create;
begin
  FItems := TBaseIcons.Create(True);
  FExtItems := TBaseIcons.Create(True);
end;

destructor TIconsManager.Destroy;
begin
  FItems.Free;
  FExtItems.Free;
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
  Icon: TFileIcon;
begin
  Result := -1;

  //Before find the icon, if it isn't exists yet, I will load it
  Icon := FindByPath(APathIcon);

  if Assigned(Icon) then
    Result := Icon.ImageIndex
  else begin
    Icon := TFileIcon.Create(APathIcon);
    try
      Result := Icon.ImageIndex;
    finally
      AddIcon(Icon);
    end;
  end;
end;

function TIconsManager.GetExtIconIndex(AExtension: string): Integer;
var
  Icon: TExtFileIcon;
begin
  Result := -1;

  //Before find the icon, if it isn't exists yet, I will load it
  Icon := FindByExt(AExtension);

  if Assigned(Icon) then
    Result := Icon.ImageIndex
  else begin
    Icon := TExtFileIcon.Create(AExtension);
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
    //ABaseIcon.LoadIcon;
  finally
    FItems.Add(ABaseIcon);
  end;
end;

procedure TIconsManager.AddExtIcon(ABaseIcon: TBaseIcon);
begin
  Assert(Assigned(ABaseIcon), 'ABaseIcon is not assigned!');

  try
    //Call ImageIndex, so I will extract the icon now
    ABaseIcon.ImageIndex;
  finally
    FExtItems.Add(ABaseIcon);
  end;
end;

function TIconsManager.GetPathTheme: string;
begin
  if FPathTheme <> '' then
    Result := FPathTheme
  else
    Result := ASuiteInstance.Paths.SuitePathCurrentTheme;
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
        Icon := FindByName(ExtractFileNameOnly(sPath));

        if not(Assigned(Icon)) then
          AddIcon(TFileIcon.Create(sPath, True));
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

function TIconsManager.FindByPath(APathFile: String): TFileIcon;
var
  Item: TBaseIcon;
begin
  //Warning: only TFileIcon!
  Result := nil;

  for Item in FItems do
  begin
    if (Item is TFileIcon) and (TFileIcon(Item).PathFile = APathFile) then
    begin
      Result := TFileIcon(Item);
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
    if (Item.Name = AName) then
    begin
      Result := Item;
      break;
    end;
  end;
end;

function TIconsManager.FindByExt(AExtension: String): TExtFileIcon;
var
  Item: TBaseIcon;
begin
  Result := nil;

  for Item in FExtItems do
  begin
    if (Item.Name = LowerCase(AExtension)) and (Item is TExtFileIcon) then
    begin
      Result := TExtFileIcon(Item);
      break;
    end;
  end;
end;

end.
