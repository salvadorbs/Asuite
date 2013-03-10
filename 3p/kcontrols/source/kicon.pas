{ @abstract(This unit provides an advanced Windows icon management
    i.e. replacement for the Graphics.TIcon component)
  @author(Tomas Krysl (tomkrysl@tkweb.eu))
  @created(9 Jan 2005)
  @lastmod(20 Jun 2010)

  Copyright © 2005 Tomas Krysl (tomkrysl@@tkweb.eu)<BR><BR>

  The purpose of the TKIcon component is to replace and expand the standard
  TIcon component provided by VCL. The TKIcon component is not based on Windows
  icon functions, but manages the icon structures by itself.
  <UL>
    <LH>Major features are:</LH>
    <LI>32-bit icons/cursors with alpha channel supported</LI>
    <LI>correct rendering in all 32-bit Windows platforms</LI>
    <LI>optional rendering of all icon/ cursors subimages</LI>
    <LI>icons/cursors can be stretched when drawn</LI>
    <LI>multiple rendering styles</LI>
    <LI>loading from file/stream, HICON, module resources, file associations</LI>
    <LI>saving to file/stream</LI>
    <LI>icon image manipulation (inserting/deleting/cropping/enlarging)</LI>
    <LI>full TPicture integration (only TPicture.Icon can't be used)</LI>
  </UL>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }
  
unit KIcon;

{$include kcontrols.inc}
{$IFNDEF TKICON_REGISTER}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

interface

{$IFDEF USE_WINAPI}

uses
  Windows, SysUtils, Classes, Graphics, KGraphics
{$IFDEF USE_PNG_SUPPORT}
 {$IFDEF FPC}
  , fpImage, GraphType, IntfGraphics
 {$ELSE}
  , PngImage  
 {$ENDIF}
{$ENDIF};

resourcestring
  { @exclude }
  SVIcons = 'Icons';
  { @exclude }
  SVCursors = 'Cursors';
  { @exclude }
  SIconAllocationError = 'Error while allocating icon data';
  { @exclude }
  SIconBitmapError = 'Invalid icon bitmap handles';
  { @exclude }
  SIconFormatError = 'Invalid icon format';
  { @exclude }
  SIconResourceError = 'Invalid icon resource';
  { @exclude }
  SIconIndexError = 'Invalid icon resource index';
  { @exclude }
  SIconInvalidModule = 'Invalid module or no icon resources';
  { @exclude }
  SIconResizingError = 'Error while resizing icon';
  { @exclude }
  SIconAssocResolveError = 'Error while resolving associated icon';

type
{$IFDEF USE_PNG_SUPPORT}
  { @exclude }
  TKIconPngObject = TKPngImage;
{$ELSE}
  { @exclude }
  TKIconPngObject = TMemoryStream; //used to store compressed PNG stream
{$ENDIF}

  { @abstract(Icon file header)
    <UL>
    <LH>Members:</LH>
    <LI><I>idReserved</I> - always 0</LI>
    <LI><I>idType</I> - 1=icon, 2=cursor</LI>
    <LI><I>idCount</I> - total number of icon images in file</LI>
    </UL>
  }
  TKIconHeader = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
  end;

  { Pointer to the icon file header structure }
  PKIconHeader = ^TKIconHeader;

  { @abstract(Helper structure identifying attributes that are different for
    icons and cursors)
    <UL>
    <LH>Members:</LH>
    <LI><I>wPlanes</I> - for icons: amount of image planes - I think that this is always 1</LI>
    <LI><I>wBitCount</I> - for icons: image color resolution</LI>
    <LI><I>wX</I> - for cursors: hot spot horizontal coordinate</LI>
    <LI><I>wY</I> - for cursors: hot spot vertical coordinate</LI>
    </UL>
  }
  TKIconCursorDirInfo = packed record
    case Integer of
    0: (
      wPlanes: Word;
      wBitCount: Word;
      );
    1: (
      wX: Word;
      wY: Word;
      );
  end;

  { @abstract(Icon/cursor directory entry. This structure decribes each
    icon/cursor image. These structures describing all images immediately follow
    the @link(TKIconHeader) structure in the icon file. After these the bitmap data
    for all images are stored (TBitmapInfoHeader, palette data, bitmap bits - XOR, AND).)
    <UL>
    <LH>Members:</LH>
    <LI><I>Width</I> - image width</LI>
    <LI><I>Height</I> - image height</LI>
    <LI><I>ColorCount</I> - number of entries in palette table</LI>
    <LI><I>Reserved</I> - not used</LI>
    <LI><I>Info</I> - different for icons/cursors</LI>
    <LI><I>dwBytesInRes</I> - total number bytes in the image including
      pallette data, XOR bits, AND bits and bitmap info header</LI>
    <LI><I>dwImageOffset</I> - position of image as offset from the beginning of file</LI>
    </UL>
  }
  TKIconCursorDirEntry = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
    Info: TKIconCursorDirInfo;
    dwBytesInRes: Longint;
    dwImageOffset: Longint;
  end;

  { Pointer to the icon/cursor directory entry }
  PKIconCursorDirEntry = ^TKIconCursorDirEntry;

  { Helper structure to typecast cursor hot spot coordinates }
  TKCursorHotSpot = packed record
    xHotSpot: Word;
    yHotSpot: Word;
  end;

  { Pointer to the cursor hot spot structure }
  PKCursorHotSpot = ^TKCursorHotSpot;

  { Helper structure for cursor specific data in resource file }
  TKCursorDir = packed record
    Width: Word;
    Height: Word;
  end;

  { Helper structure for icon specific data in resource file }
  TKIconResdir = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
  end;

  { Helper structure merging icon and cursor specific data }
  TKIconCursorInfo = packed record
    case Integer of
      0: (Icon: TKIconResdir);
      1: (Cursor: TKCursorDir);
  end;

  { @abstract(Icon/cursor directory entry as found in resource files)
    <UL>
    <LH>Members:</LH>
    <LI><I>Info</I> - structure that merges icon/cursor specific data</LI>
    <LI><I>wPlanes</I> - not used = 0</LI>
    <LI><I>wBitCount</I> - not used = 0</LI>
    <LI><I>dwBytesInRes</I> - total number of bytes in the image including
      pallette data, XOR bits, AND bits and bitmap info header</LI>
    <LI><I>wEntryName</I> - icon/cursor entry name. This number identifies the
      particular icon image in a resource file (images are stored under ICONENTRY
      key)</LI>
    </UL>
  }
  TKIconCursorDirEntryInRes = packed record
    Info: TKIconCursorInfo;
    wPlanes: Word;
    wBitCount: Word;
    dwBytesInRes: Longint;
    wEntryName: Word;
  end;

  { Pointer to the icon/cursor resource file directory entry }
  PKIconCursorDirEntryInRes = ^TKIconCursorDirEntryInRes;

  { Helper structure to access resource data }
  TKIconCursorInRes = packed record
    IH: TKIconHeader;
    Entries: array [0..MaxInt div SizeOf(TKIconCursorDirEntryInRes) - 2] of TKIconCursorDirEntryInRes;
  end;

  { Pointer to the helper structure }
  PKIconCursorInRes = ^TKIconCursorInRes;

  { Controls how the image should be aligned when they are beeing resized }
  TKIconAlignStyle = (
    { image remains aligned to the top-left corner }
    asNone,
    { image will be centered within the new boundary rectangle }
    asCenter
  );

  { Specifies the width and height of an icon or cursor image }
  TKIconDimension = record
    Width,
    Height: Integer;
  end;

  { @abstract(Specifies the GDI handles for one icon/cursor image)
    <UL>
    <LH>Members:</LH>
    <LI><I>hXOR</I> - handle to the color bitmap - icon image</LI>
    <LI><I>hAND</I> - handle to the monochrome bitmap - icon image mask</LI>
    </UL>
  }
  TKIconHandles = record
    hXOR,
    hAND: HBITMAP;
  end;

  { @abstract(Represents the internal data structure describing each icon/cursor image)
    <UL>
    <LH>Members:</LH>
    <LI><I>Width</I> - image width</LI>
    <LI><I>Height</I> - image height</LI>
    <LI><I>Bpp</I> - image color resolution</LI>
    <LI><I>BytesInRes</I> - total image data size</LI>
    <LI><I>HotSpot</I> - hot spot for a cursor</LI>
    <LI><I>iXOR</I> - pointer to the color bitmap info header + palette</LI>
    <LI><I>iXORSize</I> - size of iXOR data</LI>
    <LI><I>pXOR</I> - pointer to the color bitmap bits</LI>
    <LI><I>pXORSize</I> - size of pXOR data</LI>
    <LI><I>hXOR</I> - handle to the color bitmap - is always a DIB section</LI>
    <LI><I>pAND</I> - pointer to the monochrome (mask) bitmap bits</LI>
    <LI><I>pANDSize</I> - size of pAND data</LI>
    <LI><I>hAND</I> - handle to the monochrome bitmap - is always a DIB section</LI>
    <LI><I>PNG</I> - holds the PNG image</LI>
    </UL>
  }
  TKIconData = record
    Width: Integer;
    Height: Integer;
    Bpp: Integer;
    BytesInRes: Integer;
    Offset: Integer;
    HotSpot: TPoint;
    iXOR: PBitmapInfo;
    iXORSize: Integer;
    pXOR: Pointer;
    pXORSize: Integer;
    hXOR: HBITMAP;
    pAND: Pointer;
    pANDSize: Integer;
    hAND: HBITMAP;
    IsPNG: Boolean;
    PNG: TKIconPngObject;
  end;

  { Pointer to the internal image description structure }
  PKIconData = ^TKIconData;

  { Specifies how the icon image(s) should be rendered. This feature can be used
    along with the MaskFromColor method to implement a ‘color picker’ for a new mask construction. }
  TKIconDrawStyle = (
    { paint normally }
    idsNormal,
    { paint without applying the mask - color bitmap only }
    idsNoMask,
    { paint only the mask - monochrome bitmap only }
    idsMaskOnly,
    { paint only the alpha channel as grayscale image - only for 32 bit icon bitmaps else paint as with idsNoMask style }
    idsAlphaChannel
  );

  { KIcon main class. }
  TKIcon = class(TGraphic)
  private
    FAlignStyle: TKIconAlignStyle;
    FBpp: Integer;
    FCreating: Boolean;
    FCurrentIndex: Integer;
    FCursor: Boolean;
    FDisplayAll: Boolean;
    FDisplayHorz: Boolean;
    FIconCount: Integer;
    FIconData: array of TKIconData;
    FIconDrawStyle: TKIconDrawStyle;
    FInHandleBpp: Integer;
    FInHandleFullAlpha: Boolean;
    FMaxHeight: Integer;
    FMaxWidth: Integer;
    FOptimalIcon: Boolean;
    FOverSizeWeight: Single;
    FRequestedSize: TKIconDimension;
    FSpacing: Integer;
    FStretchEnabled: Boolean;
    function GetDimensions(Index: Integer): TKIconDimension;
    function GetHandles(Index: Integer): TKIconHandles;
    function GetHeights(Index: Integer): Integer;
    function GetHotSpot(Index: Integer): TPoint;
    function GetIconData(Index: Integer): TKIconData;
    function GetWidths(Index: Integer): Integer;
    procedure SetCurrentIndex(Value: Integer);
    procedure SetDimensions(Index: Integer; Value: TKIconDimension);
    procedure SetDisplayAll(Value: Boolean);
    procedure SetDisplayHorz(Value: Boolean);
    procedure SetHandles(Index: Integer; Value: TKIconHandles);
    procedure SetHeights(Index: Integer; Value: Integer);
    procedure SetHotSpot(Index: Integer; Value: TPoint);
    procedure SetInHandleBpp(Value: Integer);
    procedure SetIconDrawStyle(Value: TKIconDrawStyle);
    procedure SetOptimalIcon(Value: Boolean);
    procedure SetOverSizeWeight(Value: Single);
    procedure SetRequestedSize(Value: TKIconDimension);
    procedure SetSpacing(Value: Integer);
    procedure SetStretchEnabled(Value: Boolean);
    procedure SetWidths(Index: Integer; Value: Integer);
  protected
    { Overriden method - see Delphi help. Calls @link(Update) method. }
    procedure Changed(Sender: TObject); override;
    { Overriden method - see Delphi help. }
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    { Overriden method - see Delphi help. }
    function GetEmpty: Boolean; override;
    { Overriden method - see Delphi help. }
    function GetHeight: Integer; override;
    { Overriden method - see Delphi help. }
    function GetTransparent: Boolean; override;
    { Overriden method - see Delphi help. }
    function GetWidth: Integer; override;
    { Copies the bitmaps stored in Handles to the icon image identified by Index.
      If OrigBpp is True, the color resolution for the color bitmap remains unchanged,
      otherwise the value of InHandleBpp will be used. }
    procedure LoadHandles(Index: Integer; const Handles: TKIconHandles; OrigBpp: Boolean);
    { Overriden method - see Delphi help. }
    procedure SetHeight(Value: Integer); override;
    { Overriden method - see Delphi help. }
    procedure SetTransparent(Value: Boolean); override;
    { Overriden method - see Delphi help. }
    procedure SetWidth(Value: Integer); override;
    { Updates @link(MaxWidth), @link(MaxHeight) and @link(CurrentIndex)
      properties accordingly. }
    procedure Update; dynamic;
    { Resizes an icon image identified by Index to new dimensions stored in Value.
      The AlignStyle property controls the image alignment within the new rectangle. }
    procedure UpdateDim(Index: Integer; Value: TKIconDimension);
  public
    { Overriden method - see Delphi help. }
    constructor Create; override;
    { Overriden method - see Delphi help. }
    destructor Destroy; override;
    { Adds a new image to the end of the internal image list. You should always
      specify valid color and mask bitmap handles else an exception will occur. }
    procedure Add(const Handles: TKIconHandles);
    { Adds a new image from PNG to the end of the internal image list.
      No conversion is made, thus a PNG icon is added. }
    procedure AddFromPng(APngImage: TKPngImage);
    { Overriden method - see Delphi help. }
    procedure Assign(Source: TPersistent); override;
    { Clears all images so that the instance contains no icon/cursor. }
    procedure Clear; {$IFDEF FPC}override{$ELSE}dynamic{$ENDIF};
    { Copies the icon image into an alpha bitmap identified by Bitmap.
      Icon image is copied to the alpha bitmap. It icon has alpha channel
      it is copied as well.
      Bitmap size will always be matched to the icon image. }
    procedure CopyToAlphaBitmap(Index: Integer; Bitmap: TKAlphaBitmap);
    { Copies the icon image into a bitmap identified by Bitmap. Both color
      and mask image is copied to preserve true transparency. You can use this
      to pass to Glyph properties (e.g. TSpeedButton). Bitmap properties will
      always be matched to the icon image. For 32bpp icon images,
      alpha channel is copied as well. }
    procedure CopyToBitmap(Index: Integer; Bitmap: TBitmap);
  {$IFDEF USE_PNG_SUPPORT}
    { Copies the icon image into a png image identified by Png.
      It is saved always in truecolor format with alpha channel (32bpp).
      Png size will always be matched to the icon image. }
    procedure CopyToPng(Index: Integer; Png: TKPngImage);
  {$ENDIF}  
    { Creates an icon handle for use with Win32 API icon functions. The image
      identified by Index will be used for this handle. If DisplayAll is False
      and Index is out of range, CurrentIndex will be used instead. }
    function CreateHandle(Index: Integer): HICON;
    { Deletes an image identified by Index from the internal image list. }
    procedure Delete(Index: Integer);
    { Inserts an image at the position identified by Index into the internal
      image list. The existing images will be preserved and shifted accordingly. }
    procedure Insert(Index: Integer; const Handles: TKIconHandles);
  {$IFNDEF FPC}
    { Overriden method - see Delphi help. Does nothing for icons/cursors. }
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    { Loads the icon from the module associated with the file identified by FileName
      (DefaultIcon registry key). If no association can be found for the file,
      an exception will be raised and the function will try to load FileName
      as if it was a module itself. }
  {$ENDIF}
    procedure LoadFromAssocFile(const FileName: string);
    { Loads the icon from the module associated with the file extension identified
      by Extension (DefaultIcon registry key). The Extension parameter should
      contain the leading period ('.'). If no association can be found for that
      extension, an exception will be triggered. }
    procedure LoadFromAssocExtension(const Extension: string);
    { Loads the icon from Win32 API icon handle. Please keep in mind that icon bitmaps
      can't be loaded as DIBs because they are already converted to DDBs when
      accessible through HICON. So it is impossible to load the icon in it's
      native format (e.g. as stored in an *.ico file) from HICON. This function
      has been introduced only to complete the loading schemes of this class
      and you should rather use another LoadFrom... methods. The behavior of this
      function can be controlled via the InHandleBpp and InHandleFullAlpha properties.
      It is not recommended to use this function in new projects. }
    procedure LoadFromHandle(Handle: HICON);
    { Loads the icon from resources of a module identified by ModuleName.
      A valid icon resource must be specified by ID, otherwise
      an exception occurs. This function uses the LoadLibrary API function, so
      it is recommended to use the LoadFromResourceX functions to load multiple
      icons from the same module. ID is of type Word so it can’t exceed 65535. }
    procedure LoadFromModule(const ModuleName: string; ID: Word); overload;
    { Does the same thing, but with resource ID specified as string. Let's suppose
      ID = 123. Here you can pass it as a string '#123'. }
    procedure LoadFromModule(const ModuleName, ResName: string); overload;
    { This function does the same as @link(LoadFromModule), but the icon resource
      is specified by index here. The index stands for the n-th icon stored
      in the module resources. So, LoadFromModule('dummy.exe', 'MAINICON') would
      produce the same results as LoadFromModuleByIndex('dummy.exe', 0),
      provided 'MAINICON' is the first icon resource in 'dummy.exe'. }
    procedure LoadFromModuleByIndex(const ModuleName: string; Index: Integer);
    { Loads the icon from resources of a module instance identified by Instance.
      Further behavior corresponds to @link(LoadFromModule) with resource ID
      specified as integer. }
    procedure LoadFromResource(Instance: HINST; ID: Word); overload;
    { Loads the icon from resources of a module instance identified by Instance.
      Further behavior corresponds to @link(LoadFromModule) with resource ID
      specified as string. }
    procedure LoadFromResource(Instance: HINST; const ResName: string); overload;
    { Loads the icon from resources of a module instance identified by Instance.
      Further behavior corresponds to @link(LoadFromModuleByIndex). }
    procedure LoadFromResourceByIndex(Instance: HINST; Index: Integer);
    { Loads the icon from the stream. Parses the *.ico file structure.
      An overriden method. }
    procedure LoadFromStream(Stream: TStream); override;
    { Makes it possible to create a new mask bitmap for the image identified by Index.
      The new monochrome mask bitmap will be created from the color bitmap.
      Pixels of the color bitmap that match Color will be masked by the new mask,
      other pixels will be unmasked. If the Color parameter contains alpha channel,
      you should set HasAlpha to True to perform comparison with the alpha channel.
      Otherwise, only the red, green and blue channels will be compared. }
    procedure MaskFromColor(Index: Integer; Color: TColor; HasAlpha: Boolean = False);
  {$IFNDEF FPC}
    { Overriden method - see Delphi help. Does nothing for icons/cursors. }
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
  {$ENDIF}
    { Saves the icon to the stream. Assembles the *.ico file structure. An overriden method. }
    procedure SaveToStream(Stream: TStream); override;
    { Controls the icon image resizing which is performed by the UpdateDim method. }
    property AlignStyle: TKIconAlignStyle read FAlignStyle write FAlignStyle;
    { Specifies the index of the currently displayed icon image.
      If no image is loaded (no icon), the value of CurrentIndex is -1. }
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    { Indicates whether the instance of this class represents a cursor (True) or an icon (False). }
    property Cursor: Boolean read FCursor write FCursor;
    { Specifies whether all icon images (True) or a single subimage should be
      drawn (False). When True, all available icon images will be rendered. }
    property DisplayAll: Boolean read FDisplayAll write SetDisplayAll;
    { Specifies how the images should be drawn when @link(DisplayAll) is True.
      If True, the images will be drawn horizontally aligned. If False,
      the images will be drawn vertically aligned. }
    property DisplayHorz: Boolean read FDisplayHorz write SetDisplayHorz;
    { Makes it possible to read/modify the size of an icon image. }
    property Dimensions[Index: Integer]: TKIconDimension read GetDimensions write SetDimensions;
    { Makes it possible to read/modify icon image bitmaps (color and mask bitmap).
      Bitmaps that you pass will be copied and remain unchanged. When reading
      original bitmap handles are returned and thus must not be modified or released. }
    property Handles[Index: Integer]: TKIconHandles read GetHandles write SetHandles;
    { Makes it possible to read/modify the height of an icon image. }
    property Heights[Index: Integer]: Integer read GetHeights write SetHeights;
    { For a cursor, this property contains the hot spots for all cursor images. }
    property HotSpot[Index: Integer]: TPoint read GetHotSpot write SetHotSpot;
    { Returns the number of images found in this instance. }
    property IconCount: Integer read FIconCount;
    { Makes it possible to read the internal data structure of each icon image.
      A copy of the structure is returned but the pointers or handles are original
      (no copies are created) and thus must not be modified or released. }
    property IconData[Index: Integer]: TKIconData read GetIconData;
    { Affects the icon image rendering. }
    property IconDrawStyle: TKIconDrawStyle read FIconDrawStyle write SetIconDrawStyle;
    { Specifies the color resolution a DIB should have after converted from a DDB
      that has been passed to the LoadHandles method. }
    property InHandleBpp: Integer read FInHandleBpp write SetInHandleBpp;
    { Determines whether a DIB with 32 bits per pixel should have full visibility
      (alpha channel of each pixel set to 0xFF) after converted from a DDB
      that has been passed to the LoadHandles method. The alpha channel values will
      be only set to 0xFF when the current alpha channel of every pixel is zero. }
    property InHandleFullAlpha: Boolean read FInHandleFullAlpha write FInHandleFullAlpha;
    { Returns the height of the image that has the maximum height of all icon images.
      When @link(DisplayAll) is True and @link(DisplayHorz) is False, returns the
      total height of all images and spaces between them (specified by @link(Spacing)). }
    property MaxHeight: Integer read FMaxHeight;
    { Returns the width of the image that has the maximum width of all icon images.
      When both @link(DisplayAll) and @link(DisplayHorz) is True, returns the
      total width of all images and spaces between them (specified by @link(Spacing)). }
    property MaxWidth: Integer read FMaxWidth;
    { This property applies only when DisplayAll is False. It determines whether
      the icon image corresponding to the RequestedSize property and the current
      display mode color resolution (True) or the subimage specified by CurrentIndex
      (False) should be displayed. }
    property OptimalIcon: Boolean read FOptimalIcon write SetOptimalIcon;
    { Controls the decision threshold for the optimal image when OptimalIcon is True.
      The bigger the value is, the less is the probability a subimage greater than
      RequestedSize will be selected. This value is big enough by default so that
      almost always a smaller image will be selected if none with the exact size is found. }
    property OverSizeWeight: Single read FOverSizeWeight write SetOverSizeWeight;
    { Specifies the preferred image size when OptimalIcon is True.
      When OverSizeWeight is small, a greater subimage may be often selected. }
    property RequestedSize: TKIconDimension read FRequestedSize write SetRequestedSize;
    { Specifies the spacing between icon images when @link(DisplayAll) is True. }
    property Spacing: Integer read FSpacing write SetSpacing;
    { Specifies whether icon images can be stretched when drawn. This property
      was introduced perhaps only for backward compatibility with Graphics.TIcon. }
    property StretchEnabled: Boolean read FStretchEnabled write SetStretchEnabled;
    { Makes it possible to read/modify the width of an icon image. }
    property Widths[Index: Integer]: Integer read GetWidths write SetWidths;
  end;

  { This class is necessary because of the TPicture streaming. }
  TIcon = class(TKIcon);

{ Creates a bitmap from an icon object stored in application resources. }
function CreateBitmapFromResIcon(const ResName: string; ResType: PChar = RT_ICON): TBitmap;

{ Creates an alpha bitmap from an icon object stored in application resources. }
function CreateAlphaBitmapFromResIcon(const ResName: string; ResType: PChar): TKAlphaBitmap;

{ Returns the str1ucture containing hXOR and hAND bitmaps. }
function MakeHandles(hXOR, hAND: HBITMAP): TKIconHandles;

{ Returns the total number of resources of a type specified by ResType
  in a module identified by Instance. }
function GetModuleResourceCount(Instance: HINST; ResType: PChar): Integer;

{ Returns the total number of HW-independent icon resources
  in a module identified by Instance. }
function GetModuleIconCount(Instance: HINST): Integer; overload;

{ Returns the total number of HW-independent icon resources
  in a module identified by ModuleName. }
function GetModuleIconCount(const ModuleName: string): Integer; overload;

{ Integrates KIcon into TPicture. }
procedure RegisterKIcon;

{ Removes KIcon from TPicture. }
procedure UnregisterKIcon;

{$ENDIF}

implementation

{$IFDEF USE_WINAPI}

uses
  Math, Registry, KFunctions;

type
  TKMaskBitmapInfo = packed record
    Header: TBitmapInfoHeader;
    Black,
    White: TRGBQuad;
  end;

procedure FreeSubimage(PID: PKIconData);
begin
  FreeMem(PID.iXOR);
  if PID.hXOR <> 0 then DeleteObject(PID.hXOR);
  if PID.hAND <> 0 then DeleteObject(PID.hAND);
  PID.PNG.Free;
  FillChar(PID^, SizeOf(TKIconData), 0);
end;

function CalcByteWidth(Width, Bpp: Integer): Integer;
begin
  Result := DivUp(Width * Bpp, SizeOf(LongWord) shl 3) * SizeOf(LongWord);
end;

function CalcBitmapSize(Width, Height, Bpp: Integer): Integer;
begin
  Result := CalcByteWidth(Width, Bpp) * Height;
end;

procedure CalcByteWidths(Width, Bpp: Integer; out XORWidth, ANDWidth: Integer);
begin
  XORWidth := CalcByteWidth(Width, Bpp);
  ANDWidth := CalcByteWidth(Width, 1);
end;

procedure CalcBitmapSizes(Width, Height, Bpp: Integer; out XORSize, ANDSize: Integer);
begin
  XORSize := CalcBitmapSize(Width, Height, Bpp);
  ANDSize := CalcBitmapSize(Width, Height, 1);
end;

function GetPaletteSize(Bpp: Integer): Integer;
begin
  if Bpp <= 8 then
    Result := 1 shl Bpp
  else
    Result := 0;
end;

procedure QueryBitmapBits(DC: HDC; hBmp: HBITMAP; var Bits: Pointer; var Size: Integer);
var
  BInfo: Windows.TBitmap;
  BI: TBitmapInfo;
begin
  GetObject(hBmp, SizeOf(Windows.TBitmap), @BInfo);
  Size := CalcBitmapSize(BInfo.bmWidth, BInfo.bmHeight, BInfo.bmBitsPixel);
  GetMem(Bits, Size);
  FillChar(BI, SizeOf(TBitmapInfo), 0);
  with BI.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := BInfo.bmWidth;
    biHeight := BInfo.bmHeight;
    biPlanes := 1;
    biBitCount := BInfo.bmBitsPixel;
    biCompression := BI_RGB;
  end;
  GetDIBits(DC, hBmp, 0, BInfo.bmHeight, Bits, BI, DIB_RGB_COLORS);
end;

procedure CreateColorInfo(Width, Height, Bpp: Integer; var BI: PBitmapInfo; var InfoSize: Integer);
begin
  InfoSize := SizeOf(TBitmapInfoHeader) + GetPaletteSize(Bpp) * SizeOf(TRGBQuad);
  GetMem(BI, InfoSize);
  FillChar(BI^, InfoSize, 0);
  with BI.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := Bpp;
  end;
end;

procedure CreateMaskInfo(Width, Height: Integer; var BIMask: TKMaskBitmapInfo);
begin
  FillChar(BIMask, SizeOf(TKMaskBitmapInfo), 0);
  with BIMask.Header do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 1;
  end;
  Cardinal(BIMask.Black) := clBlack;
  Cardinal(BIMask.White) := clWhite;
end;

function CreateMonochromeBitmap(Width, Height: Integer): HBITMAP;
begin
  Result := GDICheck(CreateBitmap(Width, Height, 1, 1, nil));
end;

procedure MaskOrBitBlt(ACanvas: TCanvas; X, Y, Width, Height: Integer;
  DC_XOR, DC_AND: HDC; BM_XOR, BM_AND: HBITMAP;
  XORBits: PKColorRecs; XORSize: Integer;
  ANDBits: PBytes; ANDSize: Integer;
  Bpp: Integer; Style: TKIconDrawStyle);
var
  I, J, K, LAnd: Integer;
  Alpha, ByteMask: Byte;
  FreeBits: Boolean;
  Q: PBytes;
  Ps, Pd: PKColorRecs;
  BMSrc, BMDest: TKAlphaBitmap;
  R: TRect;
begin
  if Style <> idsMaskOnly then
  begin
    BMSrc := TKAlphaBitmap.Create;
    try
      BMDest := TKAlphaBitmap.Create;
      try
        R := Rect(X, Y, X + Width, Y + Height);
        BMSrc.SetSize(Width, Height);
        if Bpp = 32 then
        begin // perform alphablend
          if XORBits = nil then
          begin
            QueryBitmapBits(DC_XOR, BM_XOR, Pointer(XORBits), XORSize);
            FreeBits := True;
          end else
            FreeBits := False;
          try
            if Style = idsAlphaChannel then
            begin
              for I := 0 to Height - 1 do
              begin
                Ps := BMSrc.ScanLine[I];
                K := I * Width;
                for J := 0 to Width - 1 do
                begin
                  Alpha := 255 - XORBits[K + J].A;
                  Ps[J].R := Alpha;
                  Ps[J].G := Alpha;
                  Ps[J].B := Alpha;
                end;
              end;
            end else
            begin
              BMSrc.DrawFrom(ACanvas, R);
              for I := 0 to Height - 1 do
              begin
                Ps := @XORBits[I * Width];
                Pd := BMSrc.ScanLine[I];
                BlendLine(Ps, Pd, Width);
              end
            end
          finally
            if FreeBits then FreeMem(XORBits);
          end;
        end else
          BitBlt(BMSrc.Canvas.Handle, 0, 0, Width, Height, DC_XOR, 0, 0, SRCCOPY);
        if Style = idsNormal then
        begin
          BMDest.SetSize(Width, Height);
          BMDest.DrawFrom(ACanvas, R);
          if ANDBits = nil then
          begin
            QueryBitmapBits(DC_XOR, BM_AND, Pointer(ANDBits), ANDSize);
            FreeBits := True;
          end else
            FreeBits := False;
          if ANDBits <> nil then
          begin
            try
              LAnd := CalcByteWidth(Width, 1);
              Q := ANDBits;
              for I := 0 to Height - 1 do
              begin
                Ps := BMSrc.ScanLine[I];
                Pd := BMDest.ScanLine[I];
                ByteMask := $80;
                for J := 0 to Width - 1 do
                begin
                  if Q[J shr 3] and ByteMask <> 0 then
                    Ps[J] := Pd[J];
                  asm
                    ror ByteMask, 1
                  end;
                end;
                Inc(Cardinal(Q), LAnd);
              end;
            finally
              if FreeBits then FreeMem(ANDBits);
            end;
          end;
        end;
        BMSrc.DrawTo(ACanvas, R);
      finally
        BMDest.Free;
      end;
    finally
      BMSrc.Free;
    end;
  end else
  begin
    if DC_AND = 0 then
    begin
      DC_AND := CreateCompatibleDC(ACanvas.Handle);
      try
        SelectObject(DC_AND, BM_AND);
        BitBlt(ACanvas.Handle, X, Y, Width, Height, DC_AND, 0, 0, SrcCopy);
      finally
        DeleteDC(DC_AND);
      end;
    end else
      BitBlt(ACanvas.Handle, X, Y, Width, Height, DC_AND, 0, 0, SrcCopy);
  end;
end;

procedure FillAlphaIfNone(Pixels: PKColorRecs; Size: Integer; Alpha: Byte);
var
  I: Integer;
begin
  Size := Size shr 2;
  for I := 0 to Size - 1 do
    if Pixels[I].A <> 0 then
      Exit; // bitmap has a nonempty alpha channel, don't fill
  for I := 0 to Size - 1 do
    Pixels[I].A := Alpha;
end;

function CreateBitmapFromResIcon(const ResName: string; ResType: PChar): TBitmap;
var
  Icon: TKIcon;
  Stream: TResourceStream;
begin
  Result := TBitmap.Create;
  Icon := TKIcon.Create;
  try
    Stream := TResourceStream.Create(HInstance, ResName, ResType);
    try
      Icon.LoadFromStream(Stream);
      Icon.CopyToBitmap(Icon.CurrentIndex, Result);
    finally
      Stream.Free;
    end;
  finally
    Icon.Free;
  end;
end;

function CreateAlphaBitmapFromResIcon(const ResName: string; ResType: PChar): TKAlphaBitmap;
var
  Icon: TKIcon;
  Stream: TResourceStream;
begin
  Result := TKAlphaBitmap.Create;
  Icon := TKIcon.Create;
  try
    Stream := TResourceStream.Create(HInstance, ResName, ResType);
    try
      Icon.LoadFromStream(Stream);
      Icon.CopyToAlphaBitmap(Icon.CurrentIndex, Result);
    finally
      Stream.Free;
    end;
  finally
    Icon.Free;
  end;
end;

procedure InternalCopyToAlphaBitmap(ABitmap: TKAlphaBitmap;
  BM_XOR: HBITMAP; AndBits: PBytes; Bpp: Integer);
var
  I, J, LAnd: Integer;
  ByteMask: Byte;
  Q: PBytes;
  Ps: PKColorRecs;
  DC: HDC;
begin
  if (ABitmap <> nil) and (AndBits <> nil) and (BM_XOR <> 0) then
  begin
    DC := CreateCompatibleDC(0);
    try
      SelectObject(DC, BM_XOR);
      BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height, DC, 0, 0, SRCCOPY);
      LAnd := CalcByteWidth(ABitmap.Width, 1);
      Q := ANDBits;
      for I := 0 to ABitmap.Height - 1 do
      begin
        Ps := ABitmap.ScanLine[I];
        ByteMask := $80;
        for J := 0 to ABitmap.Width - 1 do
        begin
          if Q[J shr 3] and ByteMask <> 0 then
            Ps[J].A := 0
          else if Bpp < 32 then
            Ps[J].A := 255;    
          asm
            ror ByteMask, 1
          end;
        end;
        Inc(Cardinal(Q), LAnd);
      end;
    finally
      DeleteDC(DC);
    end;
  end;
end;

function MakeHandles(hXOR, hAND: HBITMAP): TKIconHandles;
begin
  Result.hXOR := hXOR;
  Result.hAND := hAND;
end;

function GetModuleResourceCount(Instance: HINST; ResType: PChar): Integer;

  function EnumIcons(hModule: HINST; lpType, lpName: PChar; dwParam: DWORD): BOOL; stdcall;
  begin
    Inc(PInteger(dwParam)^);
    Result := True;
  end;

begin
  Result := 0;
  EnumResourceNames(Instance, ResType, @EnumIcons, DWORD(@Result));
end;

function GetModuleIconCount(Instance: HINST): Integer;
begin
  Result := GetModuleResourceCount(Instance, RT_GROUP_ICON);
end;

function GetModuleIconCount(const ModuleName: string): Integer;
var
  Module: HINST;
begin
  Result := 0;
  Module := LoadLibraryEx(PChar(ModuleName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Module <> 0 then
  begin
    try
      Result := GetModuleIconCount(Module);
    finally
      FreeLibrary(Module);
    end;
  end;
end;

{ TKIcon }

constructor TKIcon.Create;
begin
  inherited Create;
  FCreating := True;
  try
    Transparent := True; // we are not in Graphics.pas...
  finally
    FCreating := False;
  end;
  FAlignStyle := asCenter;
  FCursor := False;
  FDisplayAll := False;
  FIconDrawStyle := idsNormal;
  FInHandleBpp := 0;
  FInHandleFullAlpha := True;
  FIconData := nil;
  FOptimalIcon := True;
  FOverSizeWeight := 1000.0; // virtually always selects a lower resolution image
  FRequestedSize.Width := 32;
  FRequestedSize.Height := 32;
  FSpacing := 2;
  FStretchEnabled := True;
  Clear;
end;

destructor TKIcon.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TKIcon.Add(const Handles: TKIconHandles);
begin
  Inc(FIconCount);
  SetLength(FIconData, FIconCount);
  FillChar(FIconData[FIconCount - 1], SizeOf(TKIconData), 0);
  LoadHandles(FIconCount - 1, Handles, True);
end;

procedure TKIcon.AddFromPng(APngImage: TKPngImage);
begin
{$IFDEF USE_PNG_SUPPORT}
  if APngImage <> nil then
  begin
    Inc(FIconCount);
    SetLength(FIconData, FIconCount);
    FillChar(FIconData[FIconCount - 1], SizeOf(TKIconData), 0);
    FIconData[FIconCount - 1].Width := APngImage.Width;
    FIconData[FIconCount - 1].Height := APngImage.Width;
    FIconData[FIconCount - 1].Bpp := 32;
    FIconData[FIconCount - 1].IsPNG := True;
    FIconData[FIconCount - 1].PNG := TKPngImage.Create;
    FIconData[FIconCount - 1].PNG.Assign(APngImage);
  end;
{$ENDIF}
end;

procedure TKIcon.Assign(Source: TPersistent);
var
  MS: TMemoryStream;
begin
  if (Source = nil) or (Source is TKIcon) then
  begin
    Clear;
    if Source <> nil then
    begin
      FAlignStyle := TKIcon(Source).AlignStyle;
      FCursor := TKIcon(Source).Cursor;
      FDisplayAll := TKIcon(Source).DisplayAll;
      FIconDrawStyle := TKIcon(Source).IconDrawStyle;
      FInHandleBpp := TKIcon(Source).InHandleBpp;
      FInHandleFullAlpha := TKIcon(Source).InHandleFullAlpha;
      FOptimalIcon := TKIcon(Source).OptimalIcon;
      FOverSizeWeight := TKIcon(Source).OverSizeWeight;
      FRequestedSize := TKIcon(Source).RequestedSize;
      FSpacing := TKIcon(Source).Spacing;
      FStretchEnabled := TKIcon(Source).StretchEnabled;
      if not TKIcon(Source).Empty then
      begin
        MS := TMemoryStream.Create;
        try
          TKIcon(Source).SaveToStream(MS);
          MS.Position := 0;
          LoadFromStream(MS);
          FCurrentIndex := TKIcon(Source).CurrentIndex;
        finally
          MS.Free;
        end;
      end else
        Changed(Self);
    end else
      Changed(Self);
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TKIcon.Changed(Sender: TObject);
begin
  Update;
  inherited;
end;

procedure TKIcon.Clear;
var
  I: Integer;
begin
  if FIconData <> nil then
  begin
    for I := 0 to FIconCount - 1 do
      FreeSubimage(@FIconData[I]);
    FIconData := nil;
  end;
  FIconCount := 0;
  Update;
end;

procedure TKIcon.CopyToAlphaBitmap(Index: Integer; Bitmap: TKAlphaBitmap);
var
  ID: TKIconData;
{$IFDEF USE_PNG_SUPPORT}
  I, J: Integer;
  C: TKColorRec;
 {$IFDEF FPC}
  IM: TLazIntfImage;
  FC: TFPColor;
 {$ENDIF}
{$ENDIF}
begin
  if (Index >= 0) and (Index < FIconCount) and (Bitmap <> nil) then
  begin
    ID := FIconData[Index];
    Bitmap.SetSize(ID.Width, ID.Height);
    Bitmap.DirectCopy := True;
    try
      if ID.IsPng then
      begin
    {$IFDEF USE_PNG_SUPPORT}
      {$IFDEF FPC}
        IM := ID.PNG.CreateIntfImage;
        try
          for I := 0 to ID.Width - 1 do
            for J := 0 to ID.Height - 1 do
            begin
              FC := IM.Colors[I, J];
              C.A := FC.alpha; C.B := FC.blue; C.R := FC.red; C.G := FC.green;
              Bitmap.Pixel[I, J] := C;
            end;
        finally
          IM.Free;
        end;
      {$ELSE}
        for I := 0 to ID.Width - 1 do
          for J := 0 to ID.Height - 1 do
          begin
            C.Value := ID.PNG.Pixels[I, J];
            C.A := ID.PNG.AlphaScanline[J][I];
            Bitmap.Pixel[I, J] := C;
          end;
      {$ENDIF}
    {$ENDIF}
      end else
        InternalCopyToAlphaBitmap(Bitmap, ID.hXOR, ID.pAND, ID.Bpp);
    finally
      Bitmap.DirectCopy := False;
    end;
  end;
end;

procedure TKIcon.CopyToBitmap(Index: Integer; Bitmap: TBitmap);
var
  DC: HDC;
  ID: TKIconData;
  Mask: TBitmap;
begin
  if (Index >= 0) and (Index < FIconCount) and (Bitmap <> nil) then
  begin
    ID := FIconData[Index];
  {$IFDEF FPC}
    Bitmap.PixelFormat := PixelFormatFromBpp(ID.Bpp);
  {$ELSE}
    Bitmap.PixelFormat := pf32bit;
  {$ENDIF}
    Bitmap.Width := ID.Width; // SetSize not supported prior Delphi 2006
    Bitmap.Height := ID.Height;
    if ID.IsPng then
  {$IFDEF USE_PNG_SUPPORT}
      Bitmap.Canvas.Draw(0, 0, ID.PNG)
  {$ENDIF}
    else
    begin
      Mask := TBitmap.Create;
      try
        Mask.MonoChrome := True;
        Mask.Width := ID.Width;
        Mask.Height := ID.Height;
        DC := CreateCompatibleDC(0);
        try
          SelectObject(DC, ID.hXOR);
          BitBlt(Bitmap.Canvas.Handle, 0, 0, ID.Width, ID.Height, DC, 0, 0, SRCCOPY);
          SelectObject(DC, ID.hAND);
          BitBlt(Mask.Canvas.Handle, 0, 0, ID.Width, ID.Height, DC, 0, 0, SRCCOPY);
          Bitmap.MaskHandle := Mask.ReleaseHandle;
        finally
          DeleteDC(DC);
        end;
      finally
        Mask.Free;
      end;
    end;
  end;
end;

{$IFDEF USE_PNG_SUPPORT}
procedure TKIcon.CopyToPng(Index: Integer; Png: TKPngImage);
var
  ID: TKIconData;
{$IFNDEF FPC}
  I, J: Integer;
  C: TKColorRec;
  Bitmap: TKAlphaBitmap;
{$ENDIF}
begin
  if (Index >= 0) and (Index < FIconCount) and (Png <> nil) then
  begin
    ID := FIconData[Index];
    if ID.IsPNG then
      Png.Assign(ID.PNG)
    else
    begin
    {$IFDEF FPC}
      Png.LoadFromBitmapHandles(ID.hXOR, ID.hAND);
    {$ELSE}
      Bitmap := TKAlphaBitmap.Create;
      try
        Bitmap.SetSize(ID.Width, ID.Height);
        Bitmap.DirectCopy := True;
        InternalCopyToAlphaBitmap(Bitmap, ID.hXOR, ID.pAND, ID.Bpp);
        Png.CreateBlank(COLOR_RGBALPHA, 8, ID.Width, ID.Height);
        for I := 0 to ID.Width - 1 do
          for J := 0 to ID.Height - 1 do
          begin
            C := Bitmap.Pixel[I, J];
            Png.Pixels[I, J] := C.Value;
            Png.AlphaScanline[J][I] := C.A;
          end;
      finally
        Bitmap.Free;
      end;
    {$ENDIF}
    end;
  end;  
end;
{$ENDIF}

function TKIcon.CreateHandle(Index: Integer): HICON;
var
  ABpp, ANDSize, XORSize: Integer;
  PID: PKIconData;
  PBI: PBitmapInfo;
  DC: HDC;
  hBmp: HBITMAP;
  ANDBits, XORBits: Pointer;
begin
  Result := 0;
  if FIconData <> nil then
  begin
    DC := GetDC(0);
    try
      ABpp := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      if ABpp <> FBpp then
        Update;
      if FDisplayAll then
      begin
        if (Index < 0) or (Index >= FIconCount) then
          Index := 0;
      end
      else if (Index < 0) or (Index >= FIconCount) then
        Index := FCurrentIndex;
      PID := @FIconData[Index];
      CalcBitmapSizes(PID.Width, PID.Height, FBpp, XORSize, ANDSize);
      GetMem(XORBits, XORSize);
      try
        GetMem(ANDBits, XORSize);
        try
          PBI := PID.iXOR;
          hBmp := GDICheck(CreateDIBitmap(DC, PBI.bmiHeader, CBM_INIT, PID.pXOR, PBI^, DIB_RGB_COLORS));
          try
            GetBitmapBits(hBmp, XORSize, XORBits); // obsolete, but the only that works fine...
            GetBitmapBits(PID.hAND, ANDSize, ANDbits);
            Result := CreateIcon(HInstance, PID.Width, PID.Height, 1, FBpp, ANDBits, XORBits);
          finally
            if hBmp <> 0 then DeleteObject(hBmp);
          end;
        finally
          FreeMem(ANDBits);
        end;
      finally
        FreeMem(XORBits);
      end;
    finally
      ReleaseDC(0, DC);
    end;
  end
end;

procedure TKIcon.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    FreeSubimage(@FIconData[Index]);
    for I := Index + 1 to FIconCount - 1 do
      FIconData[I - 1] := FIconData[I];
    Dec(FIconCount);
    SetLength(FIconData, FIconCount);
    Changed(Self);
  end;
end;

procedure TKIcon.Draw(ACanvas: TCanvas; const Rect: TRect);

  procedure Display(const P, WH: TPoint; Index: Integer);
  var
    ID: TKIconData;
    Stretch: Boolean;
    DC, DC_XOR, DC_AND: HDC;
    BM_XOR, BM_AND: HBITMAP;
    Obj, Obj_XOR, Obj_AND: HGDIObj;
  begin
    if (Index >= 0) and (Index < FIconCount) then
    begin
      ID := FIconData[Index];
      if ID.IsPNG then
      begin
      {$IFDEF USE_PNG_SUPPORT}
        ACanvas.StretchDraw(Classes.Rect(P.X, P.Y, P.X + WH.X, P.Y + WH.Y), ID.PNG);
      {$ENDIF}
      end else
      begin
        Stretch := FStretchEnabled and ((WH.X <> ID.Width) or (WH.Y <> ID.Height));
        DC := GDICheck(CreateCompatibleDC(0));
        try
          Obj := SelectObject(DC, ID.hXOR);
          if Stretch then
          begin
            DC_XOR := GDICheck(CreateCompatibleDC(DC));
            try
              BM_XOR := GDICheck(CreateCompatibleBitmap(DC, WH.X, WH.Y));
              try
                DC_AND := GDICheck(CreateCompatibleDC(DC));
                try
                  BM_AND := GDICheck(CreateMonochromeBitmap(WH.X, WH.Y));
                  try
                    Obj_XOR := SelectObject(DC_XOR, BM_XOR);
                    Obj_AND := SelectObject(DC_AND, BM_AND);
                    //SetStretchBltMode(DC_XOR, HALFTONE); //does not distribute alpha channel etc.
                    StretchBlt(DC_XOR, 0, 0, WH.X, WH.Y, DC, 0, 0, ID.Width, ID.Height, SRCCOPY);
                    SelectObject(DC, ID.hAND);
                    StretchBlt(DC_AND, 0, 0, WH.X, WH.Y, DC, 0, 0, ID.Width, ID.Height, SRCCOPY);
                    MaskOrBitBlt(ACanvas, P.X, P.Y, WH.X, WH.Y, DC_XOR, DC_AND, BM_XOR, BM_AND,
                      nil, 0, nil, 0, ID.Bpp, FIconDrawStyle);
                    SelectObject(DC_XOR, Obj_XOR);
                    SelectObject(DC_AND, Obj_AND);           
                  finally
                    DeleteObject(BM_AND);
                  end;
                finally
                  DeleteDC(DC_AND);
                end;
              finally
                DeleteObject(BM_XOR);
              end;
            finally
              DeleteDC(DC_XOR);
            end;
          end else
            MaskOrBitBlt(ACanvas, P.X, P.Y, ID.Width, ID.Height, DC, 0, ID.hXOR, ID.hAND,
              ID.pXOR, ID.pXORSize, ID.pAND, ID.pANDSize, ID.Bpp, FIconDrawStyle);
          SelectObject(DC, Obj);
        finally
          DeleteDC(DC);
        end;
      end;
    end;
  end;

var
  ABpp, AWidth, AHeight, I: Integer;
  P, WH, WH_S: TPoint;
begin
  with ACanvas do if FIconData <> nil then
  begin
    P := Rect.TopLeft;
    WH := Point(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
    if not FStretchEnabled then
    begin
      Inc(P.X, (WH.X - Width) div 2);
      Inc(P.Y, (WH.Y - Height) div 2);
    end;
    if FDisplayAll then
    begin
      AWidth := Width;
      AHeight := Height;
      WH_S := WH;
      for I := 0 to FIconCount - 1 do
      begin
        WH_S.X := FIconData[I].Width * WH.X div AWidth;
        WH_S.Y := FIconData[I].Height * WH.Y div AHeight;
        Display(P, WH_S, I);
        if FDisplayHorz then
          Inc(P.X, (FIconData[I].Width + FSpacing) * WH.X div AWidth)
        else
          Inc(P.Y, (FIconData[I].Height + FSpacing) * WH.Y div AHeight)
      end;
    end else
    begin
      ABpp := GetDeviceCaps(Handle, PLANES) * GetDeviceCaps(Handle, BITSPIXEL);
      if ABpp <> FBpp then
        Update;
      Display(P, WH, FCurrentIndex);
    end;
  end;
end;

function TKIcon.GetDimensions(Index: Integer): TKIconDimension;
begin
  Result.Width := 0; Result.Height := 0;
  if (Index >= 0) and (Index < FIconCount) then
  begin
    Result.Width := FIconData[Index].Width;
    Result.Height := FIconData[Index].Height;
  end;
end;

function TKIcon.GetEmpty: Boolean;
begin
  Result := FIconData = nil;
end;

function TKIcon.GetHandles(Index: Integer): TKIconHandles;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    Result.hXOR := FIconData[Index].hXOR;
    Result.hAND := FIconData[Index].hAND;
  end else
  begin
    Result.hXOR := 0;
    Result.hAND := 0;
  end;
end;

function TKIcon.GetHeight: Integer;
begin
  if FDisplayAll and (FIconCount > 0) then
    Result := FMaxHeight
  else
    Result := Heights[FCurrentIndex];
end;

function TKIcon.GetTransparent: Boolean;
begin
  Result := True;
end;

function TKIcon.GetHeights(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index].Height;
end;

function TKIcon.GetHotSpot(Index: Integer): TPoint;
begin
  Result.X := 0; Result.Y := 0;
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index].HotSpot;
end;

function TKIcon.GetIconData(Index: Integer): TKIconData;
begin
  FillChar(Result, SizeOf(TKIconData), #0);
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index];
end;

function TKIcon.GetWidth: Integer;
begin
  if FDisplayAll and (FIconCount > 0) then
    Result := FMaxWidth
  else
    Result := Widths[FCurrentIndex];
end;

function TKIcon.GetWidths(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index].Width;
end;

procedure TKIcon.Insert(Index: Integer; const Handles: TKIconHandles);
var
  I: Integer;
begin
  if Index >= 0 then
    if Index < FIconCount then
    begin
      Inc(FIconCount);
      SetLength(FIconData, FIconCount);
      for I := FIconCount - 2 downto Index do
        FIconData[I + 1] := FIconData[I];
      FillChar(FIconData[Index], SizeOf(TKIconData), 0);
      LoadHandles(Index, Handles, True);
    end else
      Add(Handles);
end;

{$IFNDEF FPC}
procedure TKIcon.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  // does nothing
end;
{$ENDIF}

procedure TKIcon.LoadFromHandle(Handle: HICON);
var
  Handles: TKIconHandles;
  Info: TIconInfo;
begin
  if (Handle <> 0) and GetIconInfo(Handle, Info) then
  try
    Clear;
    SetLength(FIconData, 1);
    FillChar(FIconData[0], SizeOf(TKIconData), 0);
    FIconCount := 1;
    Handles.hXOR := Info.hbmColor;
    Handles.hAND := Info.hbmMask;
    LoadHandles(0, Handles, False);
  finally
    DeleteObject(Info.hbmColor);
    DeleteObject(Info.hbmMask);
  end;
end;

procedure TKIcon.LoadFromAssocFile(const FileName: string);
begin
  try
    LoadFromAssocExtension(ExtractFileExt(FileName));
  except
    LoadFromModuleByIndex(FileName, 0);
  end;
end;

procedure TKIcon.LoadFromAssocExtension(const Extension: string);
const
  IconKey = 'DefaultIcon';
var
  Code, DashPos, I: Integer;
  Module, S, T: string;
  Reg: TRegistry;
begin
  if Extension = '' then Error(SIconAssocResolveError);
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if not Reg.KeyExists(Extension) then Error(SIconAssocResolveError);
    Reg.OpenKeyReadOnly(Extension);
    try
      S := Reg.ReadString('');
    finally
      Reg.CloseKey;
    end;
    if S = '' then Error(SIconAssocResolveError);
    S := Format('%s\%s', [S, IconKey]);
    if not Reg.KeyExists(S) then Error(SIconAssocResolveError);
    Reg.OpenKeyReadOnly(S);
    try
      S := Reg.ReadString('');
      if S = '' then Error(SIconAssocResolveError);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  DashPos := Pos(',', S);
  if DashPos > 1 then
    Module := Copy(S, 1, DashPos - 1)
  else
    Module := S;
  while CharInSetEx(Module[1], [#9, #32, '''', '"']) do System.Delete(Module, 1, 1);
  while CharInSetEx(Module[Length(Module)], [#9, #32, '''', '"']) do System.Delete(Module, Length(Module), 1);
  if Module[1] = '%' then
  begin
    System.Delete(Module, 1, 1);
    I := Pos('%', Module);
    if I >= 1 then
    begin
      T := GetEnvironmentVariable(Copy(Module, 1, I - 1));
      if T <> '' then
      begin
        System.Delete(Module, 1, I);
        Module := T + Module;
      end;
    end;
  end;
  if not FileExists(Module) then Error(SIconAssocResolveError);
  T := LowerCase(ExtractFileExt(Module));
  if T = '.ico' then
    LoadFromFile(Module)
  else
  begin
    if DashPos > 0 then
    begin
      T := Copy(S, DashPos + 1, Length(S));
      while CharInSetEx(T[1], [#9, #32]) do System.Delete(T, 1, 1);
      Val(T, I, Code);
    end else
    begin
      I := 0;
      Code := 0;
    end;
    if (Code = 0) and (I >= 0) then
      LoadFromModuleByIndex(Module, I)
    else
    begin
      if Code = 0 then
        T[1] := '#';
      LoadFromModule(Module, T);
    end;
  end;
end;

procedure TKIcon.LoadFromModule(const ModuleName: string; ID: Word);
begin
  LoadFromModule(ModuleName, Format('#%d', [ID]));
end;

procedure TKIcon.LoadFromModule(const ModuleName, ResName: string);
var
  Module: HINST;
begin
  Module := LoadLibraryEx(PChar(ModuleName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Module = 0 then Error(SIconInvalidModule);
  try
    LoadFromResource(Module, ResName);
  finally
    FreeLibrary(Module);
  end;
end;

procedure TKIcon.LoadFromModuleByIndex(const ModuleName: string; Index: Integer);
var
  Module: HINST;
begin
  Module := LoadLibraryEx(PChar(ModuleName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Module = 0 then Error(SIconInvalidModule);
  try
    LoadFromResourceByIndex(Module, Index);
  finally
    FreeLibrary(Module);
  end;
end;

procedure TKIcon.LoadFromResource(Instance: HINST; ID: Word);
begin
  LoadFromResource(Instance, Format('#%d', [ID]));
end;

procedure TKIcon.LoadFromResource(Instance: HINST; const ResName: string);
const
  ResGroup: array[Boolean] of PChar = (RT_GROUP_ICON, RT_GROUP_CURSOR);
  ResItem: array[Boolean] of PChar = (RT_ICON, RT_CURSOR);
var
  I, L, IconName, ANDSize, PalSize, XORInfoSize, XORSize: Integer;
  Masked: Boolean;
  PIC: PKIconCursorInRes;
  PBIn: PBitmapInfo;
  PID: PKIcondata;
  BIMask: TKMaskBitmapInfo;
  hGroup, hItem: HRSRC;
  hMemGroup, hMem: HGLOBAL;
  DC: HDC;
  HSign: TKImageHeaderString;
{$IFDEF USE_PNG_SUPPORT}
  Stream: TMemoryStream;
{$ENDIF}

  function GetResSize(Instance: HINST; Entry : PKIconCursorDirEntryInRes) : integer;
  var
    Rsrc: HRSRC;
    C: Cardinal;
  begin
    Result := Entry.dwBytesInRes;
    Rsrc := FindResource(Instance, Pointer(Entry.wEntryName), RT_ICON);
    if Rsrc <> 0 then
    begin
      C := SizeofResource(Instance,Rsrc);
      if C <> 0 then      // maybe if C > Result ??
        Result := C;
    end;
  end;

begin
  hGroup := FindResource(Instance, PChar(ResName), ResGroup[FCursor]);
  if hGroup = 0 then Error(SIconResourceError);
  hMemGroup := LoadResource(Instance, hGroup);
  if hMemGroup = 0 then Error(SIconResourceError);
  PIC := LockResource(hMemGroup);
  if (PIC.IH.idType = 1) and FCursor or (PIC.IH.idType = 2) and not FCursor then
    Error(SIconResourceError);
  DC := GetDC(0);
  try
    Clear;
    FIconCount := PIC.IH.idCount;
    SetLength(FIconData, FIconCount);
    FillChar(FIconData[0], SizeOf(TKIconData) * FIconCount, 0);
    for I := 0 to PIC.IH.idCount - 1 do
    begin
      IconName := PIC.Entries[I].wEntryName;
      hItem := FindResource(Instance, PChar(IconName), ResItem[FCursor]);
      if hItem = 0 then Error(SIconResourceError);
      hMem := LoadResource(Instance, hItem);
      if hMem = 0 then Error(SIconResourceError);
      PBIn := LockResource(hMem);
      try
        PID := @FIconData[I];
        try
          if FCursor then
          begin
            PID.Width := PIC.Entries[I].Info.Cursor.Width;
            PID.Height := PIC.Entries[I].Info.Cursor.Height;
            PID.HotSpot.X := PKCursorHotSpot(PBIn).xHotSpot;
            PID.HotSpot.Y := PKCursorHotSpot(PBIn).yHotSpot;
            Inc(Integer(PBIn), SizeOf(TKCursorHotSpot));
          end else
          begin
            PID.Width := PIC.Entries[I].Info.Icon.Width;
            PID.Height := PIC.Entries[I].Info.Icon.Height;
          end;
          if PID.Width = 0 then PID.Width := 256;
          if PID.Height = 0 then PID.Height := 256;
//          PID.BytesInRes := PIC.Entries[I].dwBytesInRes;    // gigo
          PID.BytesInRes := GetResSize(Instance,@PIC.Entries[I]);
          PID.Bpp := PIC.Entries[I].wBitCount;
          L := Min(8, PID.BytesInRes);
          Byte(HSign[0]) := L;
          Move(PBIn^, HSign[1], L);
          if (HSign = PNGHeader) or (HSign = MNGHeader) then
          begin
            PID.IsPNG := True;
            PID.PNG := TKIconPngObject.Create;
          {$IFDEF USE_PNG_SUPPORT}
            Stream := TMemoryStream.Create;
            try
              Stream.Write(PBIn^, PID.BytesInRes);
              Stream.Seek(0, soFromBeginning);
              PID.PNG.LoadFromStream(Stream);
            finally
              Stream.Free;
            end;
          {$ELSE}
            PID.PNG.Write(PBIn^, PID.BytesInRes);
          {$ENDIF}
          end else
          begin
            //PID.Bpp := PIC.Entries[I].wBitCount; // this is wrong in some icons
            PID.Bpp := PBIn.bmiHeader.biBitCount;
            PID.Width := PBIn.bmiHeader.biWidth;         // gigo
            PID.Height := PBIn.bmiHeader.biHeight shr 1;       // gigo
            CalcBitmapSizes(PID.Width, PID.Height, PID.Bpp, XORSize, ANDSize);
            PalSize := GetPaletteSize(PID.Bpp);
            XORInfoSize := SizeOf(TBitmapInfoHeader) + PalSize * SizeOf(TRGBQuad);
            Masked := PID.BytesInRes = XORInfoSize + XORSize + ANDSize;
            if not Masked then Error(SIconFormatError);
            GetMem(PID.iXOR, XORInfoSize);
            PID.iXORSize := XORInfoSize;
            Move(PBIn^, PID.iXOR^, XORInfoSize);
            PID.iXOR.bmiHeader.biHeight := PID.iXOR.bmiHeader.biHeight div 2;
            PID.hXOR := GDICheck(CreateDIBSection(DC, PID.iXOR^,
              DIB_RGB_COLORS, PID.pXOR, 0, 0));
            if PID.pXOR <> nil then
            begin
              Move(Pointer(Cardinal(PBIn) + Cardinal(XORInfoSize))^, PID.pXOR^, XORSize);
              PID.pXORSize := XORSize;
            end else
              Error(SIconAllocationError);
            CreateMaskInfo(PID.Width, PID.Height, BIMask);
            PID.hAND := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^,
              DIB_RGB_COLORS, PID.pAND, 0, 0));
            if PID.pAND <> nil then
            begin
              Move(Pointer(Cardinal(PBIn) + Cardinal(XORInfoSize + XORSize))^, PID.pAND^, ANDSize);
              PID.pANDSize := ANDSize;
            end else
              Error(SIconAllocationError);
          end;
        except
          FreeSubimage(PID);
          raise;
        end;
      finally
        UnlockResource(hMem); // this is not necessary, but...
        FreeResource(hMem);
      end;
    end;
  finally
    ReleaseDC(0, DC);
    UnlockResource(hMemGroup); // this is not necessary, but...
    FreeResource(hMemGroup);
  end;
  Changed(Self);
end;

type
  PCallBack = ^TCallBack;
  TCallBack = record
    I,
    Index: Integer;
    S: string;
  end;

  function EnumIcons(hModule: HINST; lpType: DWORD; lpName: PChar; dwParam: DWORD): BOOL; stdcall;
  var
    CB: PCallBack;
  begin
    CB := PCallBack(dwParam);
    if CB.I = CB.Index then
    begin
      if HiWord(Cardinal(lpName)) = 0 then
        CB.S := Format('#%d', [Cardinal(lpName)])
      else
        CB.S := lpName;
      Result := False;
    end else
      Result := True;
    Inc(CB.I);
  end;

procedure TKIcon.LoadFromResourceByIndex(Instance: HINST; Index: Integer);
var
  CB: TCallBack;
begin
  CB.I := 0;
  CB.Index := Index;
  CB.S := '';
  EnumResourceNames(Instance, RT_GROUP_ICON, @EnumIcons, DWORD(@CB));
  if CB.S <> '' then
    LoadFromResource(Instance, CB.S)
  else if CB.I = 0 then
    Error(SIconInvalidModule)
  else
    Error(SIconIndexError);
end;

procedure TKIcon.LoadFromStream(Stream: TStream);
var
  I, ANDSize, PalSize, XORInfoSize, XORSize: Integer;
  Masked: Boolean;
  PID: PKIconData;
  IH: TKIconHeader;
  II: TKIconCursorDirEntry;
  BI: TBitmapInfoHeader;
  BIMask: TKMaskBitmapInfo;
  DC: HDC;
  HSign: TKImageHeaderString;
{$IFDEF USE_PNG_SUPPORT}
  MS: TMemoryStream;
{$ENDIF}
begin
  if Stream <> nil then
  begin
    DC := GetDC(0);
    try
      Clear;
      Stream.Read(IH, SizeOf(TKIconHeader));
      FCursor := IH.idType = 2;
      FIconCount := IH.idCount;
      SetLength(FIconData, FIconCount);
      FillChar(FIconData[0], SizeOf(TKIconData) * FIconCount, 0);
      for I := 0 to FIconCount - 1 do
      begin
        PID := @FIconData[I];
        Stream.Read(II, SizeOf(TKIconCursorDirEntry));
        // for PNG read icon size here, otherwise this is overwritten when XOR bitmap is read
        PID.Width := II.Width;
        if PID.Width = 0 then PID.Width := 256;
        PID.Height := II.Height;
        if PID.Height = 0 then PID.Height := 256;
        if FCursor then
        begin
          PID.HotSpot.X := II.Info.wX;
          PID.HotSpot.Y := II.Info.wY;
        end;
        PID.BytesInRes := II.dwBytesInRes;
        PID.Offset := II.dwImageOffset;
        PID.Bpp := II.Info.wBitCount; // for PNG icons bpp is stored here
      end;
      for I := 0 to FIconCount - 1 do
      begin
        PID := @FIconData[I];
        try
          Byte(HSign[0]) := Stream.Read(HSign[1], 8);
          Stream.Seek(-8, soFromCurrent);
          if (HSign = PNGHeader) or (HSign = MNGHeader) then
          begin
            PID.IsPNG := True;
            PID.PNG := TKIconPngObject.Create;
          {$IFDEF USE_PNG_SUPPORT}
            MS := TMemoryStream.Create;
            try
              MS.CopyFrom(Stream, PID.BytesInRes); // secure icon integrity
              MS.Seek(0, soFromBeginning);
              PID.PNG.LoadFromStream(MS);
            finally
              MS.Free;
            end;
          {$ELSE}
            PID.PNG.CopyFrom(Stream, PID.BytesInRes);
          {$ENDIF}
          end else
          begin
            Stream.Read(BI, SizeOf(TBitmapInfoHeader));
            PID.Bpp := BI.biBitCount;
            PID.Width := BI.biWidth;
            PID.Height := BI.biHeight shr 1;
            PalSize := GetPaletteSize(PID.Bpp);
            CalcBitmapSizes(PID.Width, PID.Height, PID.Bpp, XORSize, ANDSize);
            XORInfoSize := SizeOf(TBitmapInfoHeader) + PalSize * SizeOf(TRGBQuad);
            Masked := PID.BytesInRes = XORInfoSize + XORSize + ANDSize;
            if not Masked then Error(SIconFormatError);
            BI.biHeight := BI.biHeight div 2;
            GetMem(PID.iXOR, XORInfoSize);
            PID.iXORSize := XORInfoSize;
            PID.iXOR.bmiHeader := BI;
            PID.iXOR.bmiHeader.biSizeImage := 0;
            Stream.Read(PID.iXOR.bmiColors, PalSize * SizeOf(TRGBQuad));
            PID.hXOR := GDICheck(CreateDIBSection(DC, PID.iXOR^,
              DIB_RGB_COLORS, PID.pXOR, 0, 0));
            if PID.pXOR <> nil then
            begin
              Stream.Read(PID.pXOR^, XORSize);
              PID.pXORSize := XORSize;
            end else
              Error(SIconAllocationError);
            CreateMaskInfo(PID.Width, PID.Height, BIMask);
            PID.hAND := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^,
              DIB_RGB_COLORS, PID.pAND, 0, 0));
            if PID.pAND <> nil then
            begin
              Stream.Read(PID.pAND^, ANDSize);
              PID.pANDSize := ANDSize;
            end else
              Error(SIconAllocationError);
          end;
        except
          FreeSubimage(PID);
          raise;
        end;
      end;
    finally
      ReleaseDC(0, DC);
    end;
    Changed(Self);
  end;
end;

procedure TKIcon.LoadHandles(Index: Integer; const Handles: TKIconHandles; OrigBpp: Boolean);
var
  ANDSize, PalSize, XORSize, XORInfoSize: Integer;
  PID: PKIconData;
  BInfo: Windows.TBitmap;
  BIMask: TKMaskBitmapInfo;
  P: Pointer;
  DC: HDC;
  hBmp: HBITMAP;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    PID := @FIconData[Index];
    if (Handles.hAND = 0) or
      (Handles.hXOR = PID.hXOR) or (Handles.hAND = PID.hXOR) or
      (Handles.hXOR = PID.hAND) or (Handles.hAND = PID.hAND) then
      Error(SIconBitmapError);
    FreeSubimage(PID);
    DC := GetDC(0);
    try
      try
        if Handles.hXOR <> 0 then
        begin
          GetObject(Handles.hXOR, SizeOf(Windows.TBitmap), @BInfo);
          PID.Height := BInfo.bmHeight;
          if OrigBpp or (FInHandleBpp = 0) then
            PID.Bpp := BInfo.bmPlanes * BInfo.bmBitsPixel
          else
            PID.Bpp := FInHandleBpp;
        end else
        begin // must be a monochrome icon - not fully tested
          GetObject(Handles.hAND, SizeOf(Windows.TBitmap), @BInfo);
          PID.Height := BInfo.bmHeight div 2;
          PID.Bpp := 1;
        end;
        PID.Width := BInfo.bmWidth;
        CalcBitmapSizes(PID.Width, PID.Height, PID.Bpp, XORSize, ANDSize);
        PalSize := GetPaletteSize(PID.Bpp);
        XORInfoSize := SizeOf(TBitmapInfoHeader) + PalSize * SizeOf(TRGBQuad);
        GetMem(PID.iXOR, XORInfoSize);
        PID.iXORSize := XORInfoSize;
        FillChar(PID.iXOR^, XORInfoSize, 0);
        PID.BytesInRes := XORInfoSize;
        PID.iXOR.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
        PID.iXOR.bmiHeader.biWidth := PID.Width;
        PID.iXOR.bmiHeader.biHeight := PID.Height;
        PID.iXOR.bmiHeader.biPlanes := 1;
        PID.iXOR.bmiHeader.biBitCount := PID.Bpp;
        PID.iXOR.bmiHeader.biCompression := BI_RGB;
        if Handles.hXOR <> 0 then hBmp := Handles.hXOR else hBmp := Handles.hAND;
        GetDIBits(DC, hBmp, 0, PID.Height, nil, PID.iXOR^, DIB_RGB_COLORS);
        PID.hXOR := GDICheck(CreateDIBSection(DC, PID.iXOR^,
          DIB_RGB_COLORS, PID.pXOR, 0, 0));
        if PID.pXOR <> nil then
        begin
          GetDIBits(DC, hBmp, 0, PID.Height, PID.pXOR,
            PID.iXOR^, DIB_RGB_COLORS);
          PID.pXORSize := XORSize;
          if (PID.Bpp = 32) and FInHandleFullAlpha then
            FillAlphaIfNone(PKColorRecs(PID.pXOR), XORSize, $FF);
          Inc(PID.BytesInRes, XORSize);
        end else
          Error(SIconAllocationError);
        CreateMaskInfo(PID.Width, PID.Height, BIMask);
        PID.hAND := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^,
          DIB_RGB_COLORS, PID.pAND, 0, 0));
        if PID.pAND <> nil then
        begin
          if Handles.hXOR <> 0 then
          begin
            GetDIBits(DC, Handles.hAND, 0, PID.Height, PID.pAND,
              PBitmapInfo(@BIMask)^, DIB_RGB_COLORS);
          end else
          begin
            GetMem(P, ANDSize * 2);
            try
              BIMask.Header.biHeight := 2 * PID.Height;
              GetDIBits(DC, Handles.hAND, 0, PID.Height * 2, P,
                PBitmapInfo(@BIMask)^, DIB_RGB_COLORS);
              Move(P^, PID.pAND^, ANDSize);
            finally
              FreeMem(P);
            end;
          end;
          PID.pANDSize := ANDSize;
          Inc(PID.BytesInRes, ANDSize);
        end else
          Error(SIconAllocationError);
      except
        FreeSubimage(PID);
        raise;
      end;
    finally
      ReleaseDC(0, DC);
    end;
    Changed(Self);
  end;
end;

procedure TKIcon.MaskFromColor(Index: Integer; Color: TColor; HasAlpha: Boolean = False);
var
  PID: PKIconData;
  DC: HDC;
  OldObj: HGDIObj;
  BM: TKAlphaBitmap;
  ByteMask: Byte;
  I, J, L, LAnd: Integer;
  ColorMask: Cardinal;
  P: PKColorRecs;
  Q: PBytes;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    Color := SwitchRGBToBGR(Color);
    PID := @FIconData[Index];
    DC := 0;
    BM := TKAlphaBitmap.Create;
    try
      BM.SetSize(PID.Width, PID.Height);
      DC := GDICheck(CreateCompatibleDC(0));
      OldObj := SelectObject(DC, PID.hXOR);
      BitBlt(BM.Canvas.Handle, 0, 0, PID.Width, PID.Height, DC, 0, 0, SRCCOPY);
      FillChar(PID.pAND^, PID.pANDSize, $FF);
      LAnd := CalcByteWidth(PID.Width, 1);
      Q := PID.pAND;
      Inc(Cardinal(Q), PID.pANDSize - LAnd);
      if HasAlpha then ColorMask := $FFFFFFFF else ColorMask := $00FFFFFF;
      for I := 0 to PID.Height - 1 do
      begin
        ByteMask := $7F;
        P := BM.ScanLine[I];
        for J := 0 to PID.Width - 1 do
        begin
          L := J shr 3;
          if P[J].Value and ColorMask <> Cardinal(Color) then
            Q[L] := Q[L] and ByteMask;
          asm
            ror ByteMask, 1
          end;
        end;
        Dec(Cardinal(Q), LAnd);
      end;
      SelectObject(DC, OldObj);
    finally
      if DC <> 0 then DeleteDC(DC);
      BM.Free;
    end;
    Changed(Self);
  end;
end;

procedure TKIcon.SaveToStream(Stream: TStream);
var
  I, Offset, RSize: Integer;
  IH: TKIconHeader;
  PID: PKIconData;
  II: TKIconCursorDirEntry;
{$IFDEF USE_PNG_SUPPORT}
  J, Delta: Integer;
  MS: TMemoryStream;
{$ENDIF}
begin
  if (Stream <> nil) and (FIconData <> nil) then
  begin
    Offset := SizeOf(TKIconHeader) + FIconCount * SizeOf(TKIconCursorDirEntry);
    IH.idReserved := 0;
    if FCursor then IH.idType := 2 else IH.idType := 1;
    IH.idCount := 0;
    for I := 0 to FIconCount - 1 do
      if (FIconData[I].iXOR <> nil) or FIconData[I].IsPNG then
        Inc(IH.idCount);
    Stream.Write(IH, SizeOf(TKIconHeader));
    for I := 0 to FIconCount - 1 do
    begin
      FillChar(II, SizeOf(TKIconCursorDirEntry), 0);  // gigo
      PID := @FIconData[I];
      if PID.IsPNG then
      begin
        II.Width := PID.Width;
        II.Height := PID.Height;
        II.ColorCount := GetPaletteSize(PID.Bpp);
        II.Info.wPlanes := 1;
        II.Info.wBitCount := PID.Bpp;
        II.dwBytesInRes := PID.BytesInRes;
        II.dwImageOffset := Offset;
        Stream.Write(II, SizeOf(TKIconCursorDirEntry));
        Inc(Offset, PID.BytesInRes);
      end
      else if PID.iXOR <> nil then
      begin
        II.Width := PID.Width;
        II.Height := PID.Height;
        II.ColorCount := GetPaletteSize(PID.Bpp);
        if FCursor then
        begin
          II.Info.wX := PID.HotSpot.X;
          II.Info.wY := PID.HotSpot.Y;
        end else
        begin
          II.Info.wPlanes := 1;
          II.Info.wBitCount := PID.Bpp;
        end;
        RSize := PID.iXORSize + PID.pXORSize + PID.pANDSize;
        II.dwBytesInRes := RSize;
        II.dwImageOffset := Offset;
        Stream.Write(II, SizeOf(TKIconCursorDirEntry));
        Inc(Offset, RSize);
      end;
    end;
    for I := 0 to FIconCount - 1 do
    begin
      PID := @FIconData[I];
      if PID.IsPNG then
      begin
      {$IFDEF USE_PNG_SUPPORT}
        MS := TMemoryStream.Create;
        try
          PID.PNG.SaveToStream(MS);
          MS.Seek(0, soFromBeginning);
          //// gigo
          if Ms.Size <> PID.BytesInRes then
          begin
            Delta := PID.BytesInRes - MS.Size;
            PID.BytesInRes := MS.Size;
            Stream.Seek(SizeOf(TKIconHeader) + I * SizeOf(TKIconCursorDirEntry), soFromBeginning);
            Stream.Read(II, SizeOf(TKIconCursorDirEntry));
            II.dwBytesInRes := PID.BytesInRes;
            Stream.Seek(-1 * SizeOf(TKIconCursorDirEntry), soFromCurrent);
            Stream.Write(II, SizeOf(TKIconCursorDirEntry));
            for J := I + 1 to FIconCount - 1 do
            begin
              Stream.Read(II, SizeOf(TKIconCursorDirEntry));
              II.dwImageOffset := II.dwImageOffset - Delta;
              Stream.Seek(-1 * SizeOf(TKIconCursorDirEntry), soFromCurrent);
              Stream.Write(II, SizeOf(TKIconCursorDirEntry));
            end;
            Stream.Seek(0,soFromEnd);
          end;
          //// end gigo
          Stream.CopyFrom(MS, PID.BytesInRes); // secure icon integrity
        finally
          MS.Free;
        end;
      {$ELSE}
        PID.PNG.Seek(0, soFromBeginning);
        Stream.CopyFrom(PID.PNG, PID.BytesInRes);
      {$ENDIF}
      end else if PID.iXOR <> nil then
      begin
        PID.iXOR.bmiHeader.biHeight := PID.iXOR.bmiHeader.biHeight * 2;
        Stream.Write(PID.iXOR^, PID.iXORSize);
        PID.iXOR.bmiHeader.biHeight := PID.iXOR.bmiHeader.biHeight div 2;
        Stream.Write(PID.pXOR^, PID.pXORSize);
        Stream.Write(PID.pAND^, PID.pANDSize);
      end;
    end;
  end;
end;

{$IFNDEF FPC}
procedure TKIcon.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
begin
  // does nothing
end;
{$ENDIF}

procedure TKIcon.SetCurrentIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < FIconCount) and (Value <> FCurrentIndex) then
  begin
    FCurrentIndex := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetDisplayAll(Value: Boolean);
begin
  if Value <> FDisplayAll then
  begin
    FDisplayAll := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetDisplayHorz(Value: Boolean);
begin
  if Value <> FDisplayHorz then
  begin
    FDisplayHorz := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetDimensions(Index: Integer; Value: TKIconDimension);
begin
  if (Index >= 0) and (Index < FIconCount) and
    (Value.Width > 0) and (Value.Height > 0) and
    (Value.Width <> Widths[Index]) and (Value.Width <> Heights[Index]) then
  begin
    UpdateDim(Index, Value);
    Changed(Self);
  end;
end;

procedure TKIcon.SetHandles(Index: Integer; Value: TKIconHandles);
begin
  LoadHandles(Index, Value, True);
end;

procedure TKIcon.SetHeight(Value: Integer);
begin
  if not FDisplayAll then
    Heights[FCurrentIndex] := Value;
end;

procedure TKIcon.SetHeights(Index: Integer; Value: Integer);
var
  D: TKIconDimension;
begin
  D.Width := Widths[Index];
  D.Height := Value;
  Dimensions[Index] := D;
end;

procedure TKIcon.SetHotSpot(Index: Integer; Value: TPoint);
var
  PID: PKIconData;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    PID := @FIconData[Index];
    if (PID.HotSpot.X <> Value.X) or (PID.HotSpot.Y <> Value.Y) then
    begin
      PID.HotSpot := Value;
      Changed(Self);
    end;
  end;
end;

procedure TKIcon.SetIconDrawStyle(Value: TKIconDrawStyle);
begin
  if Value <> FIconDrawStyle then
  begin
    FIconDrawStyle := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetInHandleBpp(Value: Integer);
begin
  if Value in [0, 1, 4, 8, 32] then
    FInHandleBpp := Value;
end;

procedure TKIcon.SetOptimalIcon(Value: Boolean);
begin
  if Value <> FOptimalIcon then
  begin
    FOptimalIcon := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetOverSizeWeight(Value: Single);
begin
  if Value <> FOverSizeWeight then
  begin
    FOverSizeWeight := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetRequestedSize(Value: TKIconDimension);
begin
  if (Value.Width > 0) and (Value.Height > 0) then
  begin
    FRequestedSize := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetStretchEnabled(Value: Boolean);
begin
  if Value <> FStretchEnabled then
  begin
    FStretchEnabled := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetTransparent(Value: Boolean);
begin
  if FCreating then
    inherited
  else
    // Ignore assignments to this property.
    // Icons are always transparent.
end;

procedure TKIcon.SetWidth(Value: Integer);
begin
  if not FDisplayAll then
    Widths[FCurrentIndex] := Value;
end;

procedure TKIcon.SetWidths(Index: Integer; Value: Integer);
var
  D: TKIconDimension;
begin
  D.Width := Value;
  D.Height := Heights[Index];
  Dimensions[Index] := D;
end;

procedure TKIcon.Update;
var
  dW, dH, BestBpp, I, MaxWeight, Weight: Integer;
  DC: HDC;
  PID: PKIconData;
begin
  FBpp := 0;
  FMaxWidth := 0;
  FMaxHeight := 0;
  if FIconData <> nil then
  begin
    DC := GetDC(0);
    try
      FBpp := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      MaxWeight := MaxInt;
      for I := 0 to FIconCount - 1 do
      begin
        PID := @FIconData[I];
        if FDisplayAll and FDisplayHorz then
        begin
          Inc(FMaxWidth, PID.Width);
          if I <> 0 then Inc(FMaxWidth, FSpacing);
        end else
          if PID.Width > FMaxWidth then FMaxWidth := PID.Width;
        if FDisplayAll and not FDisplayHorz then
        begin
          Inc(FMaxHeight, PID.Height);
          if I <> 0 then Inc(FMaxHeight, FSpacing);
        end else
          if PID.Height > FMaxHeight then FMaxHeight := PID.Height;
      end;
      if FOptimalIcon and (FIconCount >= 2) then
      begin
        FCurrentIndex := 0;
        BestBpp := FIconData[0].Bpp;
        for I := 0 to FIconCount - 1 do
        begin
          PID := @FIconData[I];
          if (PID.Bpp <= FBpp) and (PID.Bpp >= BestBpp) then
          begin
            BestBpp := PID.Bpp;
            dW := FRequestedSize.Width - PID.Width;
            dH := FRequestedSize.Height - PID.Height;
            if dW < 0 then DW := Round(-DW * FOverSizeWeight);
            if dH < 0 then dH := Round(-DH * FOverSizeWeight);
            Weight := dW + dH;
            if Weight <= MaxWeight then
            begin
              MaxWeight := Weight;
              FCurrentIndex := I;
            end;
          end;
        end;
      end  
      else if (FCurrentIndex < 0) or (FCurrentIndex >= FIconCount) then
        FCurrentIndex := 0;
    finally
      ReleaseDC(0, DC);
    end;
  end else
    FCurrentIndex := -1;
end;

procedure TKIcon.UpdateDim(Index: Integer; Value: TKIconDimension);

  procedure BitMove(const Src, Dest; BitSize, BitOffset: Integer);
  asm
    // eax: Src
    // ecx: BitSize
    // edx: Dest
    // stack: BitOffset
    // push registers that must be preserved
    push esi
    push edi
    push ebx
    // set registers for register adressing
    mov esi, eax
    mov edi, edx
    // test for scroll direction
    mov edx, BitOffset
    cmp edx, 0
    js @left
    // perform move
    mov ebx, edx
    shr ebx, 3
    add edi, ebx
    and edx, $07
    jnz @bitwise_right
    // bytewise move
    mov edx, ecx
    shr ecx, 3
    rep movsb
    and dl, $07
    jz @exit
    mov cl, dl
    mov al, [esi]
    rol eax, cl
    mov al, [edi]
    ror eax, cl
    mov [edi], al
    jmp @exit
  @bitwise_right:
    // bitwise move
    mov ebx, ecx
    mov cl, dl
    xor ch, ch
    mov dl, $7F
    ror dl, cl
    mov dh, dl
    not dh
  @R00:
    mov ah, [esi]
    ror ah, cl
    and ah, dh
    mov al, [edi]
    and al, dl
    or al, ah
    mov [edi], al
    dec ebx
    jz @exit
    inc ch
    and ch, $07
    jnz @R01
    inc esi
  @R01:
    ror dl, 1
    ror dh, 1
    test dh, $80
    jz @R00
    inc edi
    jmp @R00
  @left:
    // perform scroll
    neg edx
    mov ebx, edx
    shr ebx, 3
    add esi, ebx
    and edx, $07
    jnz @bitwise_left
    // bytewise move
    mov edx, ecx
    shr ecx, 3
    rep movsb
    and dl, $07
    jz @exit
    mov cl, dl
    mov al, [esi]
    rol eax, cl
    mov al, [edi]
    ror eax, cl
    mov [edi], al
    jmp @exit
  @bitwise_left:
    // bitwise move
    mov ebx, ecx
    mov cl, dl
    mov ch, cl
    mov dl, $7F
    mov dh, dl
    not dh
  @L00:
    mov ah, [esi]
    rol ah, cl
    and ah, dh
    mov al, [edi]
    and al, dl
    or al, ah
    mov [edi], al
    dec ebx
    jz @exit
    inc ch
    and ch, $07
    jnz @L01
    inc esi
  @L01:
    ror dl, 1
    ror dh, 1
    test dh, $80
    jz @L00
    inc edi
    jmp @L00
  @exit:
    // pop the preserved registers
    pop ebx
    pop edi
    pop esi
  end;

var
  BitOffset, J, Size, XOR1, XOR2, AND1, AND2,
  X, Y, HOffset, VOffset: Integer;
  PID: PKIconData;
  PBI: PBitmapInfoHeader;
  BIMask: TKMaskBitmapInfo;
  P: PByteArray;
  hBmp: HBITMAP;
  DC: HDC;
begin
  PID := @FIconData[Index];
  if PID.iXOR <> nil then
  begin
    PBI := PBitmapInfoHeader(PID.iXOR);
    P := nil;
    DC := GetDC(0);
    try
      try
        CalcByteWidths(PID.Width, PID.Bpp, XOR1, AND1);
        CalcByteWidths(Value.Width, PID.Bpp, XOR2, AND2);
        PBI.biWidth := Value.Width;
        PBI.biHeight := Value.Height;
        PBI.biSizeImage := XOR2 * Value.Height;
        if FAlignStyle = asCenter then
        begin
          HOffset := (Value.Width - PID.Width) div 2;
          VOffset := (Value.Height - PID.Height) div 2;
        end else
        begin
          HOffset := 0;
          VOffset := 0;
        end;
        Y := Min(PID.Height, Value.Height);
        BitOffset := HOffset * PID.Bpp;
        hBmp := GDICheck(CreateDIBSection(DC, PBitmapInfo(PBI)^, DIB_RGB_COLORS, Pointer(P), 0, 0));
        if P = nil then Error(SIconAllocationError);
        X := Min(PID.Width, Value.Width) * PID.Bpp;
        Size := XOR2 * Value.Height;
        FillChar(P^, Size, #0);
        for J := 1 to Y do
        begin
          if VOffset >= 0 then
            BitMove(PByteArray(PID.pXOR)[(PID.Height - J) * XOR1],
              P[(Value.Height - J - VOffset) * XOR2], X, BitOffset)
          else
            BitMove(PByteArray(PID.pXOR)[(PID.Height - J + VOffset) * XOR1],
              P[(Value.Height - J) * XOR2], X, BitOffset);
        end;
        DeleteObject(PID.hXOR);
        PID.pXOR := P;
        PID.pXORSize := Size;
        PID.hXOR := hBmp;
        CreateMaskInfo(PID.Width, PID.Height, BIMask);
        hBmp := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^, DIB_RGB_COLORS, Pointer(P), 0, 0));
        if P = nil then Error(SIconAllocationError);
        X := Min(PID.Width, Value.Width);
        Size := AND2 * Value.Height;
        FillChar(P^, Size, #$FF);
        for J := 1 to Y do
        begin
          if VOffset >= 0 then
            BitMove(PByteArray(PID.pAND)[(PID.Height - J) * AND1],
              P[(Value.Height - J - VOffset) * AND2], X, HOffset)
          else
            BitMove(PByteArray(PID.pAND)[(PID.Height - J + VOffset) * AND1],
              P[(Value.Height - J) * AND2], X, HOffset);
        end;
        DeleteObject(PID.hAND);
        PID.pAND := P;
        PID.pANDSize := Size;
        PID.hAND := hBmp;
        PID.Width := Value.Width;
        PID.Height := Value.Height;
      except
        FreeSubimage(PID);
        Error(SIconResizingError);
      end;
    finally
      ReleaseDC(0, DC);
    end;
  end;
end;

procedure RegisterKIcon;
begin
  TPicture.UnregisterGraphicClass(Graphics.TIcon);
  TPicture.RegisterFileFormat('ico', SVIcons, KIcon.TIcon);
  TPicture.RegisterFileFormat('cur', SVCursors, KIcon.TIcon);
end;

procedure UnregisterKIcon;
begin
  TPicture.UnregisterGraphicClass(KIcon.TIcon);
  TPicture.RegisterFileFormat('ico', SVIcons, Graphics.TIcon);
end;

{$IFDEF TKICON_REGISTER}
initialization
  RegisterKIcon;
finalization
  //not necessary, but...
  UnregisterKIcon;
{$ENDIF}

{$ENDIF}
end.
