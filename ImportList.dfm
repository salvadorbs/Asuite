object frmImportList: TfrmImportList
  Left = 749
  Top = 202
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Import list'
  ClientHeight = 304
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bvl2: TBevel
    Left = 0
    Top = 295
    Width = 281
    Height = 2
    Align = alTop
    Shape = bsBottomLine
    ExplicitWidth = 287
  end
  object bvl1: TBevel
    Left = 0
    Top = 49
    Width = 281
    Height = 2
    Align = alTop
    Shape = bsBottomLine
    ExplicitWidth = 287
  end
  object btnBack: TButton
    Left = 45
    Top = 301
    Width = 75
    Height = 23
    Caption = '< Back'
    Enabled = False
    TabOrder = 1
    OnClick = btnBackClick
  end
  object btnNext: TButton
    Left = 123
    Top = 301
    Width = 75
    Height = 23
    Caption = 'Next >'
    Default = True
    TabOrder = 2
    OnClick = btnNextClick
  end
  object btnCancel: TButton
    Left = 204
    Top = 301
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object lblTitle: TLabel
      Left = 16
      Top = 8
      Width = 257
      Height = 30
      AutoSize = False
      Caption = 'Select a launcher from which to import list and settings'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
  end
  object nbImport: TNotebook
    Left = 0
    Top = 51
    Width = 281
    Height = 244
    Align = alTop
    TabOrder = 0
    TabStop = True
    object pgLaunchers: TPage
      Left = 0
      Top = 0
      object rgrpLauncher: TRadioGroup
        Left = 12
        Top = 38
        Width = 263
        Height = 136
        Caption = 'Launcher'
        ItemIndex = 0
        Items.Strings = (
          'ASuite 2.x'
          'ASuite 1.x'
          'winPenPack Launcher 1.x')
        TabOrder = 0
      end
    end
    object pgSettings: TPage
      Left = 0
      Top = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbElements: TGroupBox
        Left = 12
        Top = 119
        Width = 263
        Height = 105
        Caption = 'Elements'
        TabOrder = 0
        object cbImportList: TCheckBox
          Left = 14
          Top = 16
          Width = 36
          Height = 19
          Caption = 'List'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbImportListClick
        end
        object cbImportSettings: TCheckBox
          Left = 14
          Top = 47
          Width = 59
          Height = 19
          Caption = 'Settings'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbImportListClick
        end
      end
      object gbFile: TGroupBox
        Left = 12
        Top = 0
        Width = 263
        Height = 105
        Caption = 'Launcher File'
        TabOrder = 1
        object lblFile: TLabel
          Left = 14
          Top = 7
          Width = 107
          Height = 13
          Caption = 'Launcher File location:'
          Color = clBtnFace
          ParentColor = False
        end
        object btnBrowse: TButton
          Left = 171
          Top = 50
          Width = 73
          Height = 22
          Caption = 'Browse...'
          TabOrder = 0
          OnClick = btnBrowseClick
        end
        object edtPathList: TEdit
          Left = 14
          Top = 23
          Width = 230
          Height = 21
          TabOrder = 1
          OnEnter = edtPathListEnter
          OnExit = edtPathListEnter
        end
      end
    end
    object pgItems: TPage
      Left = 0
      Top = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object vstListImp: TVirtualStringTree
        Left = 12
        Top = 3
        Width = 263
        Height = 210
        AnimationDuration = 0
        DragMode = dmAutomatic
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
        Images = ImagesDM.IcoImages
        ParentFont = False
        TabOrder = 0
        TextMargin = 2
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
        OnExpanding = vstListImpExpanding
        OnFreeNode = vstListImpFreeNode
        OnGetText = vstListImpGetText
        OnGetImageIndex = vstListImpGetImageIndex
        Columns = <>
      end
      object btnDeselectAll: TButton
        Left = 146
        Top = 221
        Width = 105
        Height = 17
        Caption = 'Deselect all'
        TabOrder = 1
        OnClick = btnDeselectAllClick
      end
      object btnSelectAll: TButton
        Left = 36
        Top = 221
        Width = 105
        Height = 17
        Caption = 'Select all'
        TabOrder = 2
        OnClick = btnSelectAllClick
      end
    end
    object pgProgress: TPage
      Left = 0
      Top = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblSettings: TLabel
        Left = 35
        Top = 68
        Width = 39
        Height = 13
        Caption = 'Settings'
        Color = clBtnFace
        ParentColor = False
        Layout = tlCenter
      end
      object imgSettings: TImage
        Left = 12
        Top = 68
        Width = 17
        Height = 18
      end
      object lblLauncher: TLabel
        Left = 12
        Top = 16
        Width = 87
        Height = 13
        Caption = 'From %s launcher'
        Color = clBtnFace
        ParentColor = False
      end
      object lblList: TLabel
        Left = 35
        Top = 39
        Width = 16
        Height = 13
        Caption = 'List'
        Color = clBtnFace
        ParentColor = False
        Layout = tlCenter
      end
      object imgList: TImage
        Left = 12
        Top = 39
        Width = 17
        Height = 18
      end
      object lblItems: TLabel
        Left = 12
        Top = 108
        Width = 153
        Height = 13
        Caption = 'Processing items (%d%%): %d'
        Color = clBtnFace
        ParentColor = False
      end
      object pbImport: TProgressBar
        Left = 12
        Top = 124
        Width = 255
        Height = 17
        Step = 1
        TabOrder = 0
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'All list|*.xml;*.sqlite;*.bck;*.sqbck|ASuite 2.x List (*.sqlite,' +
      ' *.sqbck)|*.sqlite;*.sqbck|ASuite 1.x List (*.xml, *.bck)|*.xml;' +
      '*.bck|winPenPack 1.x List (*.xml)|*.xml|PStart 2.x List (*.xml)|' +
      '*.xml'
    Left = 232
    Top = 4
  end
end
