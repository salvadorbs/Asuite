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
  object imgDivider2: TImage
    Left = 269
    Top = 345
    Width = 120
    Height = 2
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
    OnClick = imgPersonalPictureClick
  end
  object imgDivider1: TImage
    Left = 269
    Top = 157
    Width = 120
    Height = 2
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
    OnClick = sknbtnListClick
  end
  object sknbtnRecents: TcySkinButton
    Left = 99
    Top = 78
    Width = 70
    Height = 20
    OnClick = sknbtnRecentsClick
  end
  object sknbtnMFU: TcySkinButton
    Left = 171
    Top = 78
    Width = 70
    Height = 20
    OnClick = sknbtnMFUClick
  end
  object sknbtnEject: TcySkinButton
    Left = 255
    Top = 546
    Width = 70
    Height = 20
    OnClick = sknbtnEjectClick
  end
  object sknbtnExit: TcySkinButton
    Left = 326
    Top = 546
    Width = 70
    Height = 20
    OnClick = sknbtnExitClick
  end
  object imgDriveBackground: TImage
    Left = 116
    Top = 547
    Width = 136
    Height = 17
    Stretch = True
  end
  object imgDriveSpace: TImage
    Left = 118
    Top = 549
    Width = 131
    Height = 13
    Stretch = True
  end
  object lblDriveSpace: TLabel
    Left = 117
    Top = 546
    Width = 132
    Height = 19
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
  object btnSearch: TButtonedEdit
    Left = 15
    Top = 519
    Width = 240
    Height = 21
    Images = ImagesDM.IcoImages
    PopupMenu = frmMain.pmSearch
    RightButton.Visible = True
    TabOrder = 0
    OnKeyPress = btnSearchKeyPress
    OnRightButtonClick = btnSearchClick
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        PopupMenu = pmWindow
        ScrollBarOptions.ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect]
        OnAddToSelection = vstListAddToSelection
        OnDrawText = vstListDrawText
        OnExpanding = vstListExpanding
        OnGetText = vstGetText
        OnGetImageIndex = vstGetImageIndex
        OnKeyPress = vstKeyPress
        OnMouseMove = vstMouseMove
        OnNodeClick = vstNodeClick
        Columns = <>
      end
    end
    object tsMRU: TTabSheet
      Caption = 'tsRecents'
      ImageIndex = 1
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Images = ImagesDM.LargeIcoImages
        ParentShowHint = False
        PopupMenu = pmWindow
        ScrollBarOptions.ScrollBars = ssNone
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect]
        OnGetText = vstGetText
        OnGetImageIndex = vstGetImageLargeIndex
        OnInitNode = vstInitNode
        OnKeyPress = vstKeyPress
        OnMouseMove = vstMouseMove
        OnNodeClick = vstNodeClick
        Columns = <>
      end
    end
    object tsMFU: TTabSheet
      Caption = 'tsMostUsed'
      ImageIndex = 2
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Images = ImagesDM.LargeIcoImages
        ParentShowHint = False
        PopupMenu = pmWindow
        ScrollBarOptions.ScrollBars = ssNone
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect]
        OnGetText = vstGetText
        OnGetImageIndex = vstGetImageLargeIndex
        OnInitNode = vstInitNode
        OnKeyPress = vstKeyPress
        OnMouseMove = vstMouseMove
        OnNodeClick = vstNodeClick
        Columns = <>
      end
    end
    object tsSearch: TTabSheet
      Caption = 'tsSearch'
      ImageIndex = 3
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object vstSearch: TVirtualStringTree
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
        Images = ImagesDM.LargeIcoImages
        ParentShowHint = False
        PopupMenu = pmWindow
        ScrollBarOptions.ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect]
        OnGetText = vstGetText
        OnGetImageIndex = vstGetImageLargeIndex
        OnInitNode = vstInitNode
        OnKeyPress = vstKeyPress
        OnMouseMove = vstMouseMove
        OnNodeClick = vstNodeClick
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
    OnMessage = ApplicationEvents1Message
    Left = 280
    Top = 8
  end
  object tmrWatchFocus: TTimer
    Interval = 250
    OnTimer = tmrWatchFocusTimer
    Left = 216
    Top = 8
  end
  object pmWindow: TPopupMenu
    Images = ImagesDM.IcoImages
    Left = 184
    Top = 8
    object miRunSelectedSw: TMenuItem
      Caption = 'Run'
      Default = True
      OnClick = miRunSelectedSwClick
    end
    object miRunAs: TMenuItem
      Caption = 'Run as...'
      OnClick = miRunAsClick
    end
    object miRunAsAdmin: TMenuItem
      Caption = 'Run as admin'
      OnClick = miRunAsAdminClick
    end
    object miOpenFolderSw: TMenuItem
      Caption = 'Show application'#39's folder'
      OnClick = miOpenFolderSwClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object miProperty2: TMenuItem
      Caption = 'Properties'
      ShortCut = 116
      OnClick = miProperty2Click
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      'lblDriveSpace.*'
      'tsList.*'
      'tsMFU.*'
      'tsMRU.*'
      'tsSearch.*')
    Left = 184
    Top = 48
    LangData = {
      0E0066726D477261706869634D656E7501010000000100000007004361707469
      6F6E012C0000000D00696D674261636B67726F756E6400000C006C626C447269
      76654E616D6500000B00696D67446976696465723200000700696D674C6F676F
      0000120062766C506572736F6E616C5069637475726500001200696D67506572
      736F6E616C5069637475726500000B00696D67446976696465723100000C0073
      6B6E62746E41537569746500000D00736B6E62746E4F7074696F6E7300000F00
      736B6E62746E446F63756D656E747300000E00736B6E62746E50696374757265
      7300000B00736B6E62746E41626F757400000D00736B6E62746E4578706C6F72
      6500000C00736B6E62746E566964656F7300000B00736B6E62746E4D75736963
      00000A00736B6E62746E4C69737400000D00736B6E62746E526563656E747300
      000900736B6E62746E4D465500000B00736B6E62746E456A65637400000A0073
      6B6E62746E4578697400001200696D6744726976654261636B67726F756E6400
      000D00696D674472697665537061636500000D006C626C447269766553706163
      650000090062746E53656172636800000C007067635472656556696577730000
      060074734C697374000007007673744C6973740000050074734D525500000A00
      767374526563656E74730000050074734D465500000B007673744D6F73745573
      6564000008007473536561726368000009007673745365617263680000080074
      6D72466164657200000B004F70656E4469616C6F6731000012004170706C6963
      6174696F6E4576656E74733100000D00746D725761746368466F637573000008
      00706D57696E646F7700000F006D6952756E53656C6563746564537701010000
      0002000000070043617074696F6E0007006D6952756E41730101000000030000
      00070043617074696F6E000C006D6952756E417341646D696E01010000000400
      0000070043617074696F6E000E006D694F70656E466F6C646572537701010000
      0005000000070043617074696F6E0002004E3600000B006D6950726F70657274
      7932010100000006000000070043617074696F6E00}
  end
end
