object frmGraphicMenu: TfrmGraphicMenu
  Left = 1200
  Top = 394
  AlphaBlend = True
  AlphaBlendValue = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'ASuite Menu'
  ClientHeight = 573
  ClientWidth = 406
  Color = clBtnFace
  TransparentColor = True
  TransparentColorValue = clFuchsia
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  Scaled = False
  ScreenSnap = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgBackground: TImage
    Left = 0
    Top = 0
    Width = 406
    Height = 573
    Align = alClient
    ExplicitTop = 66
  end
  object lblDriveName: TLabel
    Left = 8
    Top = 549
    Width = 123
    Height = 13
    Margins.Bottom = 0
    AutoSize = False
    BiDiMode = bdLeftToRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    Transparent = True
  end
  object imgDriveSpace: TImage
    Left = 118
    Top = 549
    Width = 131
    Height = 13
    Picture.Data = {
      07544269746D6170AA000000424DAA0000000000000076000000280000000200
      00000D000000010004000000000034000000C40E0000C40E0000100000000000
      0000000000002D2275002F247A00342785003F3197004A3BA6005242AB005A4A
      B1006353B6006C5CBF007B6AC6008578CA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000}
    Stretch = True
  end
  object lblDriveSpace: TLabel
    Left = 117
    Top = 546
    Width = 132
    Height = 19
    Margins.Bottom = 0
    Alignment = taCenter
    AutoSize = False
    Caption = '132Mb / 2Gb (6.6%)'
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    Layout = tlCenter
  end
  object imgDivider2: TImage
    Left = 269
    Top = 345
    Width = 120
    Height = 2
    Picture.Data = {
      07544269746D61703E030000424D3E0300000000000036000000280000008100
      000002000000010018000000000008030000130B0000130B0000000000000000
      0000A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8A003B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D00}
  end
  object imgLogo: TImage
    Left = 15
    Top = 10
    Width = 160
    Height = 50
    Cursor = crSizeAll
    OnMouseDown = imgLogoMouseDown
  end
  object bvlPersonalPicture: TBevel
    Left = 347
    Top = 7
    Width = 50
    Height = 50
  end
  object imgPersonalPicture: TImage
    Left = 348
    Top = 8
    Width = 48
    Height = 48
    Cursor = crHandPoint
    Stretch = True
  end
  object imgDivider1: TImage
    Left = 269
    Top = 157
    Width = 120
    Height = 2
    Picture.Data = {
      07544269746D61703E030000424D3E0300000000000036000000280000008100
      000002000000010018000000000008030000130B0000130B0000000000000000
      0000A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8A
      A69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D
      8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA69D8AA6
      9D8AA69D8A003B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B36
      2D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B
      362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D3B362D
      3B362D3B362D3B362D00}
  end
  object sknbtnASuite: TcySkinButton
    Left = 269
    Top = 85
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnOptions: TcySkinButton
    Left = 269
    Top = 121
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnDocuments: TcySkinButton
    Left = 269
    Top = 165
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnPictures: TcySkinButton
    Left = 269
    Top = 201
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnAbout: TcySkinButton
    Left = 269
    Top = 353
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnExplore: TcySkinButton
    Left = 269
    Top = 309
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnVideos: TcySkinButton
    Left = 269
    Top = 273
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnMusic: TcySkinButton
    Left = 269
    Top = 237
    Width = 120
    Height = 30
    OnClick = OpenRightButton
  end
  object sknbtnList: TcySkinButton
    Left = 27
    Top = 78
    Width = 70
    Height = 20
  end
  object sknbtnRecents: TcySkinButton
    Left = 99
    Top = 78
    Width = 70
    Height = 20
  end
  object sknbtnMFU: TcySkinButton
    Left = 171
    Top = 78
    Width = 70
    Height = 20
  end
  object sknbtnEject: TcySkinButton
    Left = 255
    Top = 546
    Width = 70
    Height = 20
  end
  object sknbtnExit: TcySkinButton
    Left = 326
    Top = 546
    Width = 70
    Height = 20
  end
  object btnSearch: TButtonedEdit
    Left = 15
    Top = 519
    Width = 240
    Height = 21
    Images = ImagesDM.IcoImages
    RightButton.Visible = True
    TabOrder = 0
  end
  object pgcTreeViews: TPageControl
    Left = 15
    Top = 104
    Width = 240
    Height = 409
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ActivePage = tsList
    Style = tsFlatButtons
    TabOrder = 1
    object tsList: TTabSheet
      Caption = 'tsList'
      TabVisible = False
      object vstList: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 232
        Height = 399
        Align = alClient
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDrag]
        HintMode = hmHint
        Images = ImagesDM.IcoImages
        ParentShowHint = False
        ScrollBarOptions.ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect]
        OnClick = vstListClick
        OnDblClick = vstListDblClick
        OnExpanding = vstListExpanding
        OnGetText = vstListGetText
        OnGetImageIndex = vstListGetImageIndex
        ExplicitHeight = 400
        Columns = <>
      end
    end
    object tsMRU: TTabSheet
      Caption = 'tsRecents'
      ImageIndex = 1
      TabVisible = False
      object vstRecents: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 232
        Height = 399
        Align = alClient
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDrag]
        HintMode = hmHint
        Images = ImagesDM.IcoImages
        ParentShowHint = False
        ScrollBarOptions.ScrollBars = ssNone
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect]
        Columns = <>
      end
    end
    object tsMFU: TTabSheet
      Caption = 'tsMostUsed'
      ImageIndex = 2
      TabVisible = False
      object vstMostUsed: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 232
        Height = 399
        Align = alClient
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDrag]
        HintMode = hmHint
        Images = ImagesDM.IcoImages
        ParentShowHint = False
        ScrollBarOptions.ScrollBars = ssNone
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect]
        Columns = <>
      end
    end
  end
  object tmrFader: TTimer
    Interval = 20
    OnTimer = tmrFaderTimer
    Left = 248
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 312
    Top = 8
  end
  object ApplicationEvents1: TApplicationEvents
    Left = 280
    Top = 8
  end
  object tmrWatchFocus: TTimer
    Interval = 250
    Left = 216
    Top = 8
  end
end
