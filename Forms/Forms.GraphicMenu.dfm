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
    Left = 263
    Top = 544
    Width = 68
    Height = 22
    OnClick = sknbtnEjectClick
  end
  object sknbtnExit: TcySkinButton
    Left = 331
    Top = 544
    Width = 68
    Height = 22
    OnClick = sknbtnExitClick
  end
  object imgDriveBackground: TImage
    Left = 10
    Top = 544
    Width = 247
    Height = 22
    Stretch = True
  end
  object imgDriveSpace: TImage
    Left = 15
    Top = 549
    Width = 235
    Height = 13
    Stretch = True
  end
  object lblDriveSpace: TLabel
    Left = 20
    Top = 545
    Width = 236
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
  object lblDriveName: TLabel
    Left = 15
    Top = 546
    Width = 113
    Height = 19
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
    Layout = tlCenter
  end
  object imgDragSpaceHidden: TImage
    Left = 0
    Top = 0
    Width = 406
    Height = 64
    Cursor = crSizeAll
    OnMouseDown = imgLogoMouseDown
  end
  object imgPersonalPicture: TImage
    Left = 345
    Top = 10
    Width = 48
    Height = 48
    Cursor = crHandPoint
    Stretch = True
    OnClick = imgPersonalPictureClick
  end
  object imgUserFrame: TImage
    Left = 337
    Top = 2
    Width = 64
    Height = 64
    Cursor = crHandPoint
    OnClick = imgPersonalPictureClick
  end
  object btnSearch: TButtonedEdit
    Left = 15
    Top = 513
    Width = 240
    Height = 21
    Images = ImagesDM.IcoImages
    PopupMenu = frmMain.pmSearch
    RightButton.Visible = True
    TabOrder = 0
    OnChange = btnSearchChange
    OnKeyPress = btnSearchKeyPress
    OnRightButtonClick = btnSearchClick
  end
  object pgcTreeViews: TPageControl
    Left = 15
    Top = 104
    Width = 240
    Height = 405
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
        Height = 395
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
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
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
        Height = 395
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
        Height = 395
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
        Height = 395
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
      6F6E012D0000000D00696D674261636B67726F756E6400000C006C626C447269
      76654E616D6500000B00696D67446976696465723200000700696D674C6F676F
      00001200696D67506572736F6E616C5069637475726500000B00696D67446976
      696465723100000C00736B6E62746E41537569746500000D00736B6E62746E4F
      7074696F6E7300000F00736B6E62746E446F63756D656E747300000E00736B6E
      62746E506963747572657300000B00736B6E62746E41626F757400000D00736B
      6E62746E4578706C6F726500000C00736B6E62746E566964656F7300000B0073
      6B6E62746E4D7573696300000A00736B6E62746E4C69737400000D00736B6E62
      746E526563656E747300000900736B6E62746E4D465500000B00736B6E62746E
      456A65637400000A00736B6E62746E4578697400001200696D67447269766542
      61636B67726F756E6400000D00696D674472697665537061636500000D006C62
      6C447269766553706163650000090062746E53656172636800000C0070676354
      72656556696577730000060074734C697374000007007673744C697374000005
      0074734D525500000A00767374526563656E74730000050074734D465500000B
      007673744D6F7374557365640000080074735365617263680000090076737453
      656172636800000800746D72466164657200000B004F70656E4469616C6F6731
      000012004170706C69636174696F6E4576656E74733100000D00746D72576174
      6368466F63757300000800706D57696E646F7700000F006D6952756E53656C65
      637465645377010100000002000000070043617074696F6E0007006D6952756E
      4173010100000003000000070043617074696F6E000C006D6952756E41734164
      6D696E010100000004000000070043617074696F6E000E006D694F70656E466F
      6C6465725377010100000005000000070043617074696F6E0002004E3600000B
      006D6950726F706572747932010100000006000000070043617074696F6E000C
      00696D67557365724672616D6500001200696D67447261675370616365486964
      64656E0000}
  end
end
