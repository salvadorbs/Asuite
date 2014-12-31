object frmMain: TfrmMain
  Left = 611
  Top = 208
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'ASuite'
  ClientHeight = 386
  ClientWidth = 201
  Color = clBtnFace
  Constraints.MinHeight = 370
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010000000000000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    00005B4F4FFF0000000000000000000000005B4F4FFF00000000000000000000
    0000000000000000000000000000000000000000000000000000000000005B4F
    4FFFB1A3A3FF5B4F4FFFAA6600FF5B4F4FFFB1A3A3FF5B4F4FFFAA6600FFAA66
    00FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FF000000005B4F
    4FFFB1A3A3FF5B4F4FFF5B4F4FFF5B4F4FFFC5BABAFF5B4F4FFFEA8C00FFEA8C
    00FFEA8C00FFEA8C00FFEA8C00FFFF9600FFFF9600FFAA6600FF000000005B4F
    4FFFB1A3A3FFB1A3A3FFC5BABAFFC5BABAFFD8D1D1FF5B4F4FFFEA8C00FFEA8C
    00FFFF9600FFFF9600FFFF9600FFFF9600FFFF9600FFAA6600FF000000005B4F
    4FFFB1A3A3FFC5BABAFF5B4F4FFFD8D1D1FFD8D1D1FF5B4F4FFFAA6600FFAA66
    00FFAA6600FFFF9600FFFF9600FFFF9600FFFF9600FFAA6600FF000000000000
    00005B4F4FFFB1A3A3FF5B4F4FFFD8D1D1FF5B4F4FFF00000000000000000000
    0000AA6600FFFF9600FFFF9600FFFFA215FFFFA215FFAA6600FF000000000000
    00005B4F4FFFC5BABAFFD8D1D1FFD8D1D1FF5B4F4FFFAA6600FFAA6600FFAA66
    00FFAA6600FFFF9600FFFFA215FFFFAA2BFFFFA215FFAA6600FF000000000000
    0000AA6600FF5B4F4FFFD8D1D1FF5B4F4FFFEA8C00FFEA8C00FFEA8C00FFEA8C
    00FFFF9600FFFFA215FFFFA215FFFFAA2BFFFFA215FFAA6600FF000000000000
    0000AA6600FFD58000FF5B4F4FFFEA8C00FFEA8C00FFEA8C00FFFF9600FFFF96
    00FFFFA215FFFFA215FFFFAA2BFFFFA215FFFFA215FFAA6600FF000000000000
    0000AA6600FFD58000FFEA8C00FFEA8C00FFEA8C00FFAA6600FFAA6600FFAA66
    00FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FF000000000000
    0000AA6600FFEA8C00FFEA8C00FFFF9600FFFF9600FFAA6600FF000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000AA6600FFEA8C00FFEA8C00FFFF9600FFFF9600FFAA6600FFAA6600FFAA66
    00FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FF000000000000
    0000AA6600FFEA8C00FFFF9600FFFF9600FFFF9600FFFF9600FFFF9600FFFFA2
    15FFFFA215FFFFA215FFFFAA2BFFFFA215FFFFA215FFAA6600FF000000000000
    0000AA6600FFFF9600FFFF9600FFFF9600FFFF9600FFFFA215FFFFA215FFFFA2
    15FFFFAA2BFFFFAA2BFFFFAA2BFFFFA215FFFF9600FFAA6600FF000000000000
    0000AA6600FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FFAA66
    00FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FFAA6600FF000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000BBFF
    0000000100000001000000010000000100008381000080010000800100008001
    00008001000081FF000080010000800100008001000080010000FFFF0000}
  Menu = MainMenu
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pcList: TPageControl
    Left = 0
    Top = 0
    Width = 201
    Height = 386
    ActivePage = tbSearch
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    MultiLine = True
    ParentFont = False
    TabOrder = 0
    OnChange = pcListChange
    ExplicitWidth = 184
    ExplicitHeight = 340
    object tbList: TTabSheet
      Caption = 'List'
      ExplicitWidth = 176
      ExplicitHeight = 312
      object vstList: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 193
        Height = 358
        Align = alClient
        ClipboardFormats.Strings = (
          'Virtual Tree Data')
        DragMode = dmAutomatic
        EditDelay = 500
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Shell Dlg 2'
        Header.Font.Style = []
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDrag]
        HintMode = hmHint
        ParentFont = False
        ParentShowHint = False
        PopupMenu = pmWindow
        ShowHint = True
        TabOrder = 0
        TextMargin = 2
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowBackground, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        ExplicitWidth = 176
        ExplicitHeight = 312
        Columns = <>
      end
    end
    object tbSearch: TTabSheet
      Caption = 'Search'
      ExplicitWidth = 176
      ExplicitHeight = 312
      object sbtnSearch: TSpeedButton
        Left = 169
        Top = 0
        Width = 24
        Height = 23
        Align = alRight
        Anchors = []
        OnClick = btnedtSearchRightButtonClick
        ExplicitLeft = 168
      end
      object vstSearch: TVirtualStringTree
        Left = 0
        Top = 23
        Width = 193
        Height = 335
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        AnimationDuration = 0
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoVisible, hoHeaderClickAutoSort]
        HintMode = hmHint
        ParentShowHint = False
        PopupMenu = pmWindow
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
        ExplicitWidth = 176
        ExplicitHeight = 289
        Columns = <
          item
            Position = 0
            Width = 90
            WideText = 'Name'
          end
          item
            Position = 1
            Width = 90
            WideText = 'Category'
          end>
      end
      object edtSearch: TEdit
        Left = 0
        Top = 0
        Width = 166
        Height = 21
        Align = alLeft
        Anchors = [akLeft, akTop, akRight]
        Constraints.MaxHeight = 21
        PopupMenu = pmSearch
        TabOrder = 1
        OnKeyPress = btnedtSearchKeyPress
        ExplicitWidth = 149
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 32
    object miFile: TMenuItem
      Caption = 'File'
      object miSaveList1: TMenuItem
        Caption = 'Save list'
        ShortCut = 16467
        OnClick = miSaveListClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miOptions1: TMenuItem
        Caption = 'Options'
        OnClick = miOptionsClick
      end
      object mniScanFolder: TMenuItem
        Caption = 'Scan for files...'
        OnClick = mniScanFolderClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miImportList: TMenuItem
        Caption = 'Import list...'
        ShortCut = 113
        OnClick = miImportListClick
      end
      object miExportList: TMenuItem
        Caption = 'Export list...'
        OnClick = miExportListClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miExit1: TMenuItem
        Caption = 'Exit'
        OnClick = miExitClick
      end
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      OnClick = miEditClick
      object miSortList: TMenuItem
        Caption = 'Sort list'
        OnClick = miSortListClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miAddCat1: TMenuItem
        Caption = 'Add category'
        OnClick = AddCategory
      end
      object miAddSw1: TMenuItem
        Caption = 'Add software'
        OnClick = AddSoftware
      end
      object miAddFolder1: TMenuItem
        Caption = 'Add folder'
        OnClick = AddFolder
      end
      object miAddSeparator1: TMenuItem
        Caption = 'Add separator'
        OnClick = miAddSeparator2Click
      end
      object miN11: TMenuItem
        Caption = '-'
      end
      object miCut1: TMenuItem
        Caption = 'Cut'
      end
      object miCopy1: TMenuItem
        Caption = 'Copy'
      end
      object miPaste1: TMenuItem
        Caption = 'Paste'
      end
      object miDelete1: TMenuItem
        Caption = 'Delete'
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object miProperty1: TMenuItem
        Caption = 'Properties'
      end
    end
    object miHelp: TMenuItem
      Caption = 'Help'
      object miCheckUpdates: TMenuItem
        Caption = 'Check updates'
      end
      object MenuItem3: TMenuItem
        Caption = '-'
      end
      object miStatistics: TMenuItem
        Caption = 'Statistics'
        OnClick = miStatisticsClick
      end
      object MenuItem2: TMenuItem
        Caption = '-'
      end
      object miInfoASuite: TMenuItem
        Caption = 'About ASuite'
        ShortCut = 112
        OnClick = miInfoASuiteClick
      end
    end
  end
  object pmWindow: TPopupMenu
    OnPopup = pmWindowPopup
    Left = 80
    Top = 32
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
    object N9: TMenuItem
      Caption = '-'
    end
    object miSortItems: TMenuItem
      Caption = 'Sort category'#39's items'
      OnClick = miSortItemsClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object miAddCat2: TMenuItem
      Caption = 'Add category'
      ShortCut = 114
      OnClick = AddCategory
    end
    object miAddSw2: TMenuItem
      Caption = 'Add software'
      ShortCut = 115
      OnClick = AddSoftware
    end
    object miAddFolder2: TMenuItem
      Caption = 'Add folder'
      OnClick = AddFolder
    end
    object miAddSeparator2: TMenuItem
      Caption = 'Add separator'
      OnClick = miAddSeparator2Click
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object miCut2: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = miCut2Click
    end
    object miCopy2: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = miCopy2Click
    end
    object miPaste2: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = miPaste2Click
    end
    object miDelete2: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object miProperty2: TMenuItem
      Caption = 'Properties'
      ShortCut = 116
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.xml'
    Filter = 'ASuite List File (*.sqlite)|*.sqlite|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 112
    Top = 32
  end
  object pmSearch: TPopupMenu
    Left = 48
    Top = 32
    object miSearchName: TMenuItem
      Caption = 'Name'
      Checked = True
      RadioItem = True
      OnClick = ChangeSearchTextHint
    end
    object miSearchExePath: TMenuItem
      Tag = 1
      Caption = 'Executable path'
      RadioItem = True
      OnClick = ChangeSearchTextHint
    end
    object miSearchIconPath: TMenuItem
      Tag = 2
      Caption = 'Icon path'
      RadioItem = True
      OnClick = ChangeSearchTextHint
    end
    object miSearchWorkingDirPath: TMenuItem
      Tag = 3
      Caption = 'Working directory path'
      RadioItem = True
      OnClick = ChangeSearchTextHint
    end
    object miSearchParameters: TMenuItem
      Tag = 4
      Caption = 'Parameters'
      RadioItem = True
      OnClick = ChangeSearchTextHint
    end
  end
  object tmScheduler: TTimer
    Enabled = False
    OnTimer = tmSchedulerTimer
    Left = 144
    Top = 32
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      'SaveDialog1.*'
      'vstList.*')
    Left = 16
    Top = 80
    LangData = {
      070066726D4D61696E010100000001000000070043617074696F6E0141000000
      060070634C6973740000060074624C6973740101000000020000000700436170
      74696F6E0007007673744C697374000008007462536561726368010100000004
      000000070043617074696F6E000A007362746E53656172636800000900767374
      53656172636800000900656474536561726368000008004D61696E4D656E7500
      0006006D6946696C65010100000005000000070043617074696F6E000B006D69
      536176654C69737431010100000006000000070043617074696F6E0002004E34
      00000A006D694F7074696F6E7331010100000007000000070043617074696F6E
      000D006D6E695363616E466F6C64657201010000000800000007004361707469
      6F6E0002004E3100000C006D69496D706F72744C697374010100000009000000
      070043617074696F6E000C006D694578706F72744C69737401010000000A0000
      00070043617074696F6E0002004E32000007006D69457869743101010000000B
      000000070043617074696F6E0006006D694564697401010000000C0000000700
      43617074696F6E000A006D69536F72744C69737401010000000D000000070043
      617074696F6E0002004E33000009006D694164644361743101010000000E0000
      00070043617074696F6E0008006D6941646453773101010000000F0000000700
      43617074696F6E000C006D69416464466F6C6465723101010000001000000007
      0043617074696F6E000F006D69416464536570617261746F7231010100000011
      000000070043617074696F6E0005006D694E3131000006006D69437574310101
      00000012000000070043617074696F6E0007006D69436F707931010100000013
      000000070043617074696F6E0008006D69506173746531010100000014000000
      070043617074696F6E0009006D6944656C657465310101000000150000000700
      43617074696F6E0002004E3800000B006D6950726F7065727479310101000000
      16000000070043617074696F6E0006006D6948656C7001010000001700000007
      0043617074696F6E000E006D69436865636B5570646174657301010000001800
      0000070043617074696F6E0009004D656E754974656D3300000C006D69537461
      74697374696373010100000019000000070043617074696F6E0009004D656E75
      4974656D3200000C006D69496E666F41537569746501010000001A0000000700
      43617074696F6E000800706D57696E646F7700000F006D6952756E53656C6563
      746564537701010000001B000000070043617074696F6E0007006D6952756E41
      7301010000001C000000070043617074696F6E000C006D6952756E417341646D
      696E01010000001D000000070043617074696F6E000E006D694F70656E466F6C
      646572537701010000001E000000070043617074696F6E0002004E3900000B00
      6D69536F72744974656D7301010000001F000000070043617074696F6E000200
      4E35000009006D6941646443617432010100000020000000070043617074696F
      6E0008006D69416464537732010100000021000000070043617074696F6E000C
      006D69416464466F6C64657232010100000022000000070043617074696F6E00
      0F006D69416464536570617261746F7232010100000023000000070043617074
      696F6E0003004E3130000006006D694375743201010000002400000007004361
      7074696F6E0007006D69436F707932010100000025000000070043617074696F
      6E0008006D69506173746532010100000026000000070043617074696F6E0009
      006D6944656C65746532010100000027000000070043617074696F6E0002004E
      3600000B006D6950726F70657274793201010000002800000007004361707469
      6F6E000B00536176654469616C6F673100000800706D53656172636800000C00
      6D695365617263684E616D6501010000002B000000070043617074696F6E000F
      006D695365617263684578655061746801010000002C00000007004361707469
      6F6E0010006D6953656172636849636F6E5061746801010000002D0000000700
      43617074696F6E0016006D69536561726368576F726B696E6744697250617468
      01010000002E000000070043617074696F6E0012006D69536561726368506172
      616D657465727301010000002F000000070043617074696F6E000B00746D5363
      686564756C65720000}
  end
end
