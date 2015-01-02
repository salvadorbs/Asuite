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
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  Scaled = False
  ScreenSnap = True
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgBackground: TImage
    Left = 0
    Top = 0
    Width = 406
    Height = 573
    Align = alClient
  end
  object imgDivider2: TImage
    Left = 269
    Top = 345
    Width = 120
    Height = 2
  end
  object imgLogo: TImage
    Left = 15
    Top = 9
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
    Left = 15
    Top = 79
    Width = 78
    Height = 20
    OnClick = sknbtnListClick
  end
  object sknbtnRecents: TcySkinButton
    Left = 96
    Top = 79
    Width = 78
    Height = 20
    OnClick = sknbtnRecentsClick
  end
  object sknbtnMFU: TcySkinButton
    Left = 177
    Top = 79
    Width = 78
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
    Left = 14
    Top = 544
    Width = 236
    Height = 22
    Stretch = True
  end
  object lblDriveSpace: TLabel
    Left = 80
    Top = 546
    Width = 176
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
  object imgDragSpaceHidden: TImage
    Left = 0
    Top = 0
    Width = 406
    Height = 68
    Cursor = crSizeAll
    OnMouseDown = imgLogoMouseDown
  end
  object edtSearch: TButtonedEdit
    Left = 15
    Top = 510
    Width = 240
    Height = 21
    PopupMenu = frmMain.pmSearch
    RightButton.Visible = True
    TabOrder = 1
    OnChange = edtSearchChange
    OnRightButtonClick = btnSearchClick
  end
  object vstList: TVirtualStringTree
    Left = 15
    Top = 105
    Width = 240
    Height = 396
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
    ParentShowHint = False
    PopupMenu = pmWindow
    ScrollBarOptions.ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toInitOnSave, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toPopupMode, toShowBackground, toShowDropmark, toThemeAware, toUseBlendedImages, toAlwaysHideSelection, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toRightClickSelect]
    Columns = <>
  end
  object tmrFader: TTimer
    Interval = 20
    OnTimer = tmrFaderTimer
    Left = 288
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Left = 224
    Top = 16
  end
  object ApplicationEvents1: TApplicationEvents
    OnDeactivate = ApplicationEvents1Deactivate
    Left = 192
    Top = 16
  end
  object pmWindow: TPopupMenu
    OnPopup = pmWindowPopup
    Left = 256
    Top = 16
    object miRunSelectedSw: TMenuItem
      Caption = 'Run'
      Default = True
    end
    object miRunAs: TMenuItem
      Caption = 'Run as...'
    end
    object miRunAsAdmin: TMenuItem
      Caption = 'Run as admin'
    end
    object miOpenFolderSw: TMenuItem
      Caption = 'Show application'#39's folder'
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
    Left = 120
    Top = 176
    LangData = {
      0E0066726D477261706869634D656E7501010000000100000007004361707469
      6F6E01250000000D00696D674261636B67726F756E6400000C006C626C447269
      76654E616D6500000B00696D67446976696465723200000700696D674C6F676F
      00001200696D67506572736F6E616C5069637475726500000B00696D67446976
      696465723100000D00736B6E62746E4F7074696F6E7300000F00736B6E62746E
      446F63756D656E747300000E00736B6E62746E506963747572657300000B0073
      6B6E62746E41626F757400000D00736B6E62746E4578706C6F726500000C0073
      6B6E62746E566964656F7300000B00736B6E62746E4D7573696300000A00736B
      6E62746E4C69737400000D00736B6E62746E526563656E747300000900736B6E
      62746E4D465500000B00736B6E62746E456A65637400000A00736B6E62746E45
      78697400001200696D6744726976654261636B67726F756E6400000D00696D67
      4472697665537061636500000D006C626C447269766553706163650000090065
      6474536561726368000007007673744C69737400000800746D72466164657200
      000B004F70656E4469616C6F6731000012004170706C69636174696F6E457665
      6E74733100000800706D57696E646F7700000F006D6952756E53656C65637465
      645377010100000002000000070043617074696F6E0007006D6952756E417301
      0100000003000000070043617074696F6E000C006D6952756E417341646D696E
      010100000004000000070043617074696F6E000E006D694F70656E466F6C6465
      725377010100000005000000070043617074696F6E0002004E3600000B006D69
      50726F706572747932010100000006000000070043617074696F6E000C00696D
      67557365724672616D6500001200696D6744726167537061636548696464656E
      00000C00736B6E62746E41537569746500000D00746D72436865636B4974656D
      730000}
  end
  object tmrCheckItems: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrCheckItemsTimer
    Left = 120
    Top = 128
  end
end
