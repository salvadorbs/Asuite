object frmMain: TfrmMain
  Left = 611
  Top = 208
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'ASuite'
  ClientHeight = 340
  ClientWidth = 184
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
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pcList: TPageControl
    Left = 0
    Top = 0
    Width = 184
    Height = 340
    ActivePage = tbList
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
        Width = 176
        Height = 312
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
        Images = ImagesDM.IcoImages
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
        OnClick = RunSingleClick
        OnCompareNodes = vstListCompareNodes
        OnDblClick = RunDoubleClick
        OnDragOver = vstListDragOver
        OnDragDrop = vstListDragDrop
        OnEditing = vstListEditing
        OnExpanding = vstListExpanding
        OnFreeNode = vstListFreeNode
        OnGetText = vstListGetText
        OnPaintText = vstListPaintText
        OnGetImageIndex = vstListGetImageIndex
        OnKeyPress = vstListKeyPress
        OnLoadNode = vstListLoadNode
        OnNewText = vstListNewText
        OnSaveNode = vstListSaveNode
        Columns = <>
      end
    end
    object tbSearch: TTabSheet
      Caption = 'Search'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object sbtnSearch: TSpeedButton
        Left = 152
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
        Width = 176
        Height = 289
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        AnimationDuration = 0
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoVisible]
        HintMode = hmHint
        ParentShowHint = False
        PopupMenu = pmWindow
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
        OnClick = RunSingleClick
        OnCompareNodes = vstSearchCompareNodes
        OnDblClick = RunDoubleClick
        OnGetText = vstSearchGetText
        OnGetImageIndex = vstSearchGetImageIndex
        OnHeaderClick = vstSearchHeaderClick
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
        Width = 149
        Height = 21
        Align = alLeft
        Anchors = [akLeft, akTop, akRight]
        Constraints.MaxHeight = 21
        PopupMenu = pmSearch
        TabOrder = 1
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
        OnClick = DeleteSwCat
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object miProperty1: TMenuItem
        Caption = 'Properties'
        OnClick = ShowProperty
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
      OnClick = OpenFolderSw
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
      OnClick = DeleteSwCat
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object miProperty2: TMenuItem
      Caption = 'Properties'
      ShortCut = 116
      OnClick = ShowProperty
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
end
