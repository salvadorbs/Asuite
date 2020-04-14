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
        ParentFont = False
        ParentShowHint = False
        PopupMenu = pmWindow
        ShowHint = True
        TabOrder = 0
        TextMargin = 2
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoChangeScale]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight, toFullRowDrag, toEditOnClick]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowBackground, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        Columns = <>
      end
    end
    object tbSearch: TTabSheet
      Caption = 'Search'
      object vstSearch: TVirtualStringTree
        Left = 0
        Top = 24
        Width = 193
        Height = 332
        Align = alTop
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
      object btnedtSearch: TButtonedEdit
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 193
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        DoubleBuffered = False
        LeftButton.DropDownMenu = pmSearch
        LeftButton.Visible = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        RightButton.Visible = True
        ShowHint = True
        TabOrder = 0
        TextHint = 'Name'
        OnChange = btnedtSearchChange
        OnKeyPress = btnedtSearchKeyPress
        OnRightButtonClick = btnedtSearchRightButtonClick
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
    end
    object miHelp: TMenuItem
      Caption = 'Help'
      object miCheckUpdates: TMenuItem
        Caption = 'Check updates'
        OnClick = miCheckUpdatesClick
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
    object mniRunItem: TMenuItem
      Action = actRunItem
      Default = True
      ShortCut = 120
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
      ShortCut = 114
    end
    object mniAddSoftware: TMenuItem
      Tag = 1
      Action = actAddSoftware
      ShortCut = 115
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
      ShortCut = 16472
    end
    object mniCopy: TMenuItem
      Action = actCopy
      ShortCut = 16451
    end
    object mniPaste: TMenuItem
      Action = actPaste
      ShortCut = 16470
    end
    object mniDelete: TMenuItem
      Action = actDelete
      ShortCut = 46
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object mniProperty: TMenuItem
      Action = actProperty
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
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      'SaveDialog1.*'
      'vstList.*')
    Left = 16
    Top = 80
    LangData = {
      070066726D4D61696E010100000001000000070043617074696F6E0144000000
      060070634C6973740000060074624C6973740101000000020000000700436170
      74696F6E0007007673744C697374000008007462536561726368010100000004
      000000070043617074696F6E000900767374536561726368000008004D61696E
      4D656E75000006006D6946696C65010100000005000000070043617074696F6E
      000B006D69536176654C69737431010100000006000000070043617074696F6E
      0002004E3400000A006D694F7074696F6E733101010000000700000007004361
      7074696F6E000D006D6E695363616E466F6C6465720101000000080000000700
      43617074696F6E0002004E3100000C006D69496D706F72744C69737401010000
      0009000000070043617074696F6E000C006D694578706F72744C697374010100
      00000A000000070043617074696F6E0002004E32000007006D69457869743101
      010000000B000000070043617074696F6E0006006D694564697401010000000C
      000000070043617074696F6E000F006D6E69536F72744361744974656D730000
      02004E33000009006D6E6941646443617400000E006D6E69416464536F667477
      61726500000C006D6E69416464466F6C64657200000F006D6E69416464536570
      617261746F72000005006D694E3131000006006D6E69437574000007006D6E69
      436F7079000008006D6E695061737465000009006D6E6944656C657465000002
      004E3800000B006D6E6950726F7065727479000006006D6948656C7001010000
      0017000000070043617074696F6E000E006D69436865636B5570646174657301
      0100000018000000070043617074696F6E0009004D656E754974656D3300000C
      006D6953746174697374696373010100000019000000070043617074696F6E00
      09004D656E754974656D3200000C006D69496E666F4153756974650101000000
      1A000000070043617074696F6E000800706D57696E646F7700000B0053617665
      4469616C6F673100000800706D53656172636800000C006D695365617263684E
      616D6501010000002B000000070043617074696F6E000F006D69536561726368
      4578655061746801010000002C000000070043617074696F6E0010006D695365
      6172636849636F6E5061746801010000002D000000070043617074696F6E0016
      006D69536561726368576F726B696E674469725061746801010000002E000000
      070043617074696F6E0012006D69536561726368506172616D65746572730101
      0000002F000000070043617074696F6E000B00416374696F6E4C697374310000
      0A0061637452756E4974656D010100000030000000070043617074696F6E000C
      0061637452756E41734974656D010100000031000000070043617074696F6E00
      110061637452756E417341646D696E4974656D01010000003200000007004361
      7074696F6E0011006163744F70656E466F6C6465724974656D01010000003300
      0000070043617074696F6E000F00616374536F72744361744974656D73010100
      000034000000070043617074696F6E0009006163744164644361740101000000
      37000000070043617074696F6E00060061637443757401010000003900000007
      0043617074696F6E000700616374436F707901010000003A0000000700436170
      74696F6E000800616374506173746501010000003B000000070043617074696F
      6E00090061637444656C65746501010000003C000000070043617074696F6E00
      0B0061637450726F706572747901010000003D000000070043617074696F6E00
      0E00616374416464536F66747761726501010000003E00000007004361707469
      6F6E000C00616374416464466F6C64657201010000003F000000070043617074
      696F6E000F00616374416464536570617261746F720101000000400000000700
      43617074696F6E000A006D6E6952756E4974656D00000C006D6E6952756E4173
      4974656D000011006D6E6952756E417341646D696E4974656D000011006D6E69
      4F70656E466F6C6465724974656D000002004E3900000D00746D72436865636B
      4974656D7300000B00616374536F72744C697374010100000041000000070043
      617074696F6E000B006D6E69536F72744C69737400000C0062746E6564745365
      6172636801010000004200000008005465787448696E7400}
  end
  object ActionList1: TActionList
    Left = 132
    Top = 192
    object actRunItem: TAction
      Caption = 'Run'
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
      OnExecute = actAddItem
      OnUpdate = actAddItemUpdate
    end
    object actCut: TAction
      Caption = 'Cut'
      OnExecute = actCutExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actCopy: TAction
      Caption = 'Copy'
      OnExecute = actCopyExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actPaste: TAction
      Caption = 'Paste'
      OnExecute = actPasteExecute
      OnUpdate = actPasteUpdate
    end
    object actDelete: TAction
      Caption = 'Delete'
      OnExecute = actDeleteExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actProperty: TAction
      Caption = 'Property'
      OnExecute = actPropertyExecute
      OnUpdate = actCutCopyDeleteUpdate
    end
    object actAddSoftware: TAction
      Tag = 1
      Caption = 'Add software...'
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
