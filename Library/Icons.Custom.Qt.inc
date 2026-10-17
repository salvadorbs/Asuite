{ Shared Qt5/Qt6 custom icon implementation.

  Included by Library/qt5/Icons.Custom.pas and Library/qt6/Icons.Custom.pas.
  Both units must declare:
    TQtCustomIcon = class(TBaseIcon)
      function QIconToHBitmap(AIcon: QIconH; ASize: Types.TSize): HBITMAP;
      function ExtractIcon(const sIconName: WideString; ALargeImage: Boolean): TBitmap;
    end;
  Requires qt5 or qt6 plus qtobjects in scope (same API names). }

function TQtCustomIcon.QIconToHBitmap(AIcon: QIconH; ASize: Types.TSize
  ): HBITMAP;
var
  AImage: QImageH;
  APixmap: QPixmapH;
begin
  APixmap := QPixmap_create();
  QIcon_pixmap(AIcon, APixmap, Types.PSize(@ASize));

  AImage := QImage_create();
  QPixmap_toImage(APixmap, AImage);
  QPixmap_destroy(APixmap);

  Result := HBitmap(TQtImage.Create(AImage));
end;

function TQtCustomIcon.ExtractIcon(const sIconName: WideString;
  ALargeImage: Boolean): TBitmap;
var
  QIcon: QIconH;
  Size: Types.TSize;
begin
  Result := TBitmap.Create;

  if ALargeImage then
    Size := Types.Size(ICON_SIZE_LARGE, ICON_SIZE_LARGE)
  else
    Size := Types.Size(ICON_SIZE_SMALL, ICON_SIZE_SMALL);

  QIcon := QIcon_Create();
  try
    if ASuiteManager.IconsManager.CheckSystemIconName(sIconName) then
    begin
      QIcon_fromTheme(QIcon, @sIconName);
      Result.Handle := QIconToHBitmap(QIcon, Size);
    end;
  finally
    QIcon_destroy(QIcon);
  end;
end;

function TQtCustomIcon.GetIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  iconName: String;
  bmp: TBitmap;
begin
  iconName := ASuiteManager.IconsManager.GetSystemIconName(APathFile);

  bmp := ExtractIcon(iconName, AWantLargeIcon);
  try
    Result := TBGRABitmap.Create(bmp);
  finally
    bmp.Free;
  end;
end;
