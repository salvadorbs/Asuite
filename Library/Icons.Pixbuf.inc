{ Shared PixBuf -> TBitmap conversion for Gtk2/Gtk3 custom icons.

  Included by Library/gtk2/Icons.Custom.pas and Library/gtk3/Icons.Custom.pas.
  Both units must declare:
    TGtkCustomIcon = class(TBaseIcon)
      function PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;
  Requires GraphType, IntfGraphics and a gdk pixbuf unit in scope. }

function TGtkCustomIcon.PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;
var
  width, height, rowstride, n_channels, i, j: Integer;
  pixels: PByte;
  pSrc: PByte;
  pDst: PLongWord;
  BmpData: TLazIntfImage;
  hasAlphaChannel: Boolean;
  QueryFlags: TRawImageQueryFlags = [riqfRGB];
  Description: TRawImageDescription;
  ARawImage: TRawImage;
begin
  Result := nil;

  n_channels:= gdk_pixbuf_get_n_channels(Pixbuf);

  if ((n_channels <> 3) and (n_channels <> 4)) or  // RGB or RGBA
     (gdk_pixbuf_get_colorspace(pixbuf) <> GDK_COLORSPACE_RGB) or
     (gdk_pixbuf_get_bits_per_sample(pixbuf) <> 8) then Exit;

  width:= gdk_pixbuf_get_width(Pixbuf);
  height:= gdk_pixbuf_get_height(Pixbuf);
  rowstride:= gdk_pixbuf_get_rowstride(Pixbuf);
  pixels:= PByte(gdk_pixbuf_get_pixels(Pixbuf));
  hasAlphaChannel:= gdk_pixbuf_get_has_alpha(Pixbuf);

  if hasAlphaChannel then
    Include(QueryFlags, riqfAlpha);

  BmpData := TLazIntfImage.Create(width, height, QueryFlags);
  try
    BmpData.CreateData;
    Description := BmpData.DataDescription;

    pDst := PLongWord(BmpData.PixelData);
    for j:= 0 to Height - 1 do
    begin
      pSrc := PByte(pixels) + j * rowstride;
      for i:= 0 to Width - 1 do
      begin
        pDst^ := pSrc[0] shl Description.RedShift +
                 pSrc[1] shl Description.GreenShift +
                 pSrc[2] shl Description.BlueShift;

        if hasAlphaChannel then
          pDst^ := pDst^ + pSrc[3] shl Description.AlphaShift;

        Inc(pSrc, n_channels);
        Inc(pDst);
      end;
    end;

    Result := TBitmap.Create;

    BmpData.GetRawImage(ARawImage, True);
    // Simply change raw image owner without data copy
    Result.LoadFromRawImage(ARawImage, True);

    if not hasAlphaChannel then
      Result.Transparent := True;

  finally
    BmpData.Free;
  end;
end;
