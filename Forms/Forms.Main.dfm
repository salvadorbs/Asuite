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
  Menu = MainMenu
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcList: TPageControl
    Left = 0
    Top = 0
    Width = 201
    Height = 386
    ActivePage = tbList
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Images = dmImages.ilSmallIcons
    MultiLine = True
    ParentFont = False
    TabOrder = 0
    OnChange = pcListChange
    object tbList: TTabSheet
      Caption = 'List'
      object vstList: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 193
        Height = 357
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
        Images = dmImages.ilSmallIcons
        ParentFont = False
        ParentShowHint = False
        PopupMenu = pmWindow
        ShowHint = True
        TabOrder = 0
        TextMargin = 2
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight, toFullRowDrag, toEditOnClick]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        Columns = <>
      end
    end
    object tbSearch: TTabSheet
      Caption = 'Search'
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
        Height = 334
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
        Images = dmImages.ilSmallIcons
        ParentShowHint = False
        PopupMenu = pmWindow
        ShowHint = True
        TabOrder = 1
        TreeOptions.PaintOptions = [toHideFocusRect, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
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
        TabOrder = 0
        OnKeyPress = btnedtSearchKeyPress
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
      object mniRunItem: TMenuItem
        Action = actRunItem
        Default = True
      end
      object mniRunAsItem: TMenuItem
        Action = actRunAsItem
      end
      object mniRunAsAdminItem: TMenuItem
        Action = actRunAsAdminItem
      end
      object mniOpenFolderItem: TMenuItem
        Action = actOpenFolderItem
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mniSortCatItems: TMenuItem
        Action = actSortCatItems
      end
      object mniSortList: TMenuItem
        Action = actSortList
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mniAddCat: TMenuItem
        Action = actAddCat
      end
      object mniAddSoftware: TMenuItem
        Tag = 1
        Action = actAddSoftware
      end
      object mniAddFolder: TMenuItem
        Tag = 2
        Action = actAddFolder
      end
      object mniAddSeparator: TMenuItem
        Tag = 3
        Action = actAddSeparator
      end
      object miN11: TMenuItem
        Caption = '-'
      end
      object mniCut: TMenuItem
        Action = actCut
      end
      object mniCopy: TMenuItem
        Action = actCopy
      end
      object mniPaste: TMenuItem
        Action = actPaste
      end
      object mniDelete: TMenuItem
        Action = actDelete
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mniProperty: TMenuItem
        Action = actProperty
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
    Left = 80
    Top = 32
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
      070066726D4D61696E010100000001000000070043617074696F6E0146000000
      060070634C6973740000060074624C6973740101000000020000000700436170
      74696F6E0007007673744C697374000008007462536561726368010100000004
      000000070043617074696F6E000A007362746E53656172636800000900767374
      53656172636800000900656474536561726368000008004D61696E4D656E7500
      010100000002004E35000006006D6946696C6501010000000500000007004361
      7074696F6E000B006D69536176654C6973743101010000000600000007004361
      7074696F6E0002004E3400000A006D694F7074696F6E73310101000000070000
      00070043617074696F6E000D006D6E695363616E466F6C646572010100000008
      000000070043617074696F6E0002004E3100000C006D69496D706F72744C6973
      74010100000009000000070043617074696F6E000C006D694578706F72744C69
      737401010000000A000000070043617074696F6E0002004E32000007006D6945
      7869743101010000000B000000070043617074696F6E0006006D694564697401
      010000000C000000070043617074696F6E000F006D6E69536F72744361744974
      656D73000002004E33000009006D6E6941646443617400000E006D6E69416464
      536F66747761726500000C006D6E69416464466F6C64657200000F006D6E6941
      6464536570617261746F72000005006D694E3131000006006D6E694375740000
      07006D6E69436F7079000008006D6E695061737465000009006D6E6944656C65
      7465000002004E3800000B006D6E6950726F7065727479000006006D6948656C
      70010100000017000000070043617074696F6E000E006D69436865636B557064
      61746573010100000018000000070043617074696F6E0009004D656E75497465
      6D3300000C006D69537461746973746963730101000000190000000700436170
      74696F6E0009004D656E754974656D3200000C006D69496E666F415375697465
      01010000001A000000070043617074696F6E000800706D57696E646F7700000B
      00536176654469616C6F673100000800706D53656172636800000C006D695365
      617263684E616D6501010000002B000000070043617074696F6E000F006D6953
      65617263684578655061746801010000002C000000070043617074696F6E0010
      006D6953656172636849636F6E5061746801010000002D000000070043617074
      696F6E0016006D69536561726368576F726B696E674469725061746801010000
      002E000000070043617074696F6E0012006D69536561726368506172616D6574
      65727301010000002F000000070043617074696F6E000B00746D536368656475
      6C657200000B00416374696F6E4C6973743100000A0061637452756E4974656D
      010100000030000000070043617074696F6E000C0061637452756E4173497465
      6D010100000031000000070043617074696F6E00110061637452756E41734164
      6D696E4974656D010100000032000000070043617074696F6E0011006163744F
      70656E466F6C6465724974656D010100000033000000070043617074696F6E00
      0F00616374536F72744361744974656D73010100000034000000070043617074
      696F6E000900616374416464436174010100000037000000070043617074696F
      6E000600616374437574010100000039000000070043617074696F6E00070061
      6374436F707901010000003A000000070043617074696F6E0008006163745061
      73746501010000003B000000070043617074696F6E00090061637444656C6574
      6501010000003C000000070043617074696F6E000B0061637450726F70657274
      7901010000003D000000070043617074696F6E000E00616374416464536F6674
      7761726501010000003E000000070043617074696F6E000C0061637441646446
      6F6C64657201010000003F000000070043617074696F6E000F00616374416464
      536570617261746F72010100000040000000070043617074696F6E000A006D6E
      6952756E4974656D00000C006D6E6952756E41734974656D000011006D6E6952
      756E417341646D696E4974656D000011006D6E694F70656E466F6C6465724974
      656D000002004E3900000D00746D72436865636B4974656D7300000B00616374
      536F72744C697374010100000041000000070043617074696F6E000B006D6E69
      536F72744C6973740000}
  end
  object ActionList1: TActionList
    Left = 132
    Top = 192
    object actRunItem: TAction
      Caption = 'Run'
      ShortCut = 120
      OnExecute = actRunItemExecute
      OnUpdate = actRunItemUpdate
    end
    object actRunAsItem: TAction
      Tag = 1
      Caption = 'Run as...'
      OnExecute = actRunItemExecute
      OnUpdate = actRunItemUpdate
    end
    object actRunAsAdminItem: TAction
      Tag = 2
      Caption = 'Run as admin'
      OnExecute = actRunItemExecute
      OnUpdate = actRunItemUpdate
    end
    object actOpenFolderItem: TAction
      Tag = 3
      Caption = 'Show application'#39's folder'
      OnExecute = actRunItemExecute
      OnUpdate = actRunItemUpdate
    end
    object actSortCatItems: TAction
      Caption = 'Sort category'#39's items'
      OnExecute = actSortCatItemsExecute
      OnUpdate = actSortCatItemsUpdate
    end
    object actAddCat: TAction
      Caption = 'Add category...'
      ShortCut = 114
      OnExecute = actAddItem
      OnUpdate = actAddItemUpdate
    end
    object actCut: TAction
      Caption = 'Cut'
      ShortCut = 16472
      OnExecute = actCutExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actCopy: TAction
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = actCopyExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actPaste: TAction
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = actPasteExecute
      OnUpdate = actPasteUpdate
    end
    object actDelete: TAction
      Caption = 'Delete'
      ShortCut = 46
      OnExecute = actDeleteExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actProperty: TAction
      Caption = 'Property'
      ShortCut = 116
      OnExecute = actPropertyExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actAddSoftware: TAction
      Tag = 1
      Caption = 'Add software...'
      ShortCut = 115
      OnExecute = actAddItem
      OnUpdate = actAddItemUpdate
    end
    object actAddFolder: TAction
      Tag = 2
      Caption = 'Add folder...'
      OnExecute = actAddItem
      OnUpdate = actAddItemUpdate
    end
    object actAddSeparator: TAction
      Tag = 3
      Caption = 'Add separator...'
      OnExecute = actAddItem
      OnUpdate = actAddItemUpdate
    end
    object actSortList: TAction
      Caption = 'Sort list'
      OnExecute = actSortListExecute
      OnUpdate = actSortListUpdate
    end
  end
  object tmrCheckItems: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrCheckItemsTimer
    Left = 144
    Top = 80
  end
end
