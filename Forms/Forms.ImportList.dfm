object frmImportList: TfrmImportList
  Left = 749
  Top = 202
  BorderStyle = bsDialog
  Caption = 'Import list'
  ClientHeight = 330
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bvl1: TBevel
    Left = 0
    Top = 293
    Width = 287
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object bvl2: TBevel
    Left = 0
    Top = 49
    Width = 287
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object btnBack: TButton
    Left = 45
    Top = 301
    Width = 75
    Height = 23
    Caption = '< Back'
    Enabled = False
    TabOrder = 2
    OnClick = btnBackClick
  end
  object btnNext: TButton
    Left = 123
    Top = 301
    Width = 75
    Height = 23
    Caption = 'Next >'
    Default = True
    TabOrder = 3
    OnClick = btnNextClick
  end
  object btnCancel: TButton
    Left = 204
    Top = 301
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 287
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 8
      Width = 257
      Height = 30
      AutoSize = False
      Caption = 'Placeholder text'
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
  object pgcImport: TPageControl
    Left = 0
    Top = 51
    Width = 287
    Height = 242
    ActivePage = tsList
    Align = alTop
    Style = tsButtons
    TabOrder = 1
    object tsAskFileList: TTabSheet
      Caption = 'tsAskFileList'
      ImageIndex = 1
      TabVisible = False
      OnShow = tsAskFileListShow
      object gbFile: TGroupBox
        Left = 13
        Top = 75
        Width = 253
        Height = 82
        Caption = 'File List'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object lblFile: TLabel
          Left = 11
          Top = 24
          Width = 107
          Height = 13
          Caption = 'Launcher File location:'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object edtPathList: TJvFilenameEdit
          Left = 11
          Top = 40
          Width = 230
          Height = 21
          AddQuotes = False
          Filter = 
            'All list supported|*.xml;*.sqlite;*.bck;*.sqbck|ASuite 2.x List ' +
            '(*.sqlite, *.sqbck)|*.sqlite;*.sqbck|ASuite 1.x List (*.xml, *.b' +
            'ck)|*.xml;*.bck|winPenPack 1.x List (*.xml)|*.xml|PStart 2.x Lis' +
            't (*.xml)|*.xml'
          DialogOptions = [ofHideReadOnly, ofEnableSizing]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Text = ''
          OnChange = edtPathListChange
        end
      end
    end
    object tsList: TTabSheet
      Caption = 'tsItems'
      ImageIndex = 2
      TabVisible = False
      OnShow = tsListShow
      object vstListImp: TVirtualStringTree
        Left = 12
        Top = 3
        Width = 253
        Height = 204
        AnimationDuration = 0
        ClipboardFormats.Strings = (
          'CSV'
          'HTML Format'
          'Plain text'
          'Rich Text Format'
          'Rich Text Format Without Objects'
          'Unicode text'
          'Virtual Tree Data')
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
        ParentFont = False
        ScrollBarOptions.ScrollBars = ssVertical
        TabOrder = 0
        TextMargin = 2
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
        OnChecked = vstListImpChecked
        Columns = <>
      end
      object btnSelectAll: TButton
        Left = 32
        Top = 213
        Width = 105
        Height = 17
        Caption = 'Select all'
        TabOrder = 1
        OnClick = btnSelectAllClick
      end
      object btnDeselectAll: TButton
        Left = 143
        Top = 213
        Width = 105
        Height = 17
        Caption = 'Deselect all'
        TabOrder = 2
        OnClick = btnDeselectAllClick
      end
    end
  end
  object XMLDocument1: TXMLDocument
    Left = 192
    Top = 8
    DOMVendorDesc = 'MSXML'
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Filter'
      'rgrpLauncher.Items'
      'tsLaunchers.*'
      'tsList.*'
      'tsProgress.*'
      'tsSettings.*'
      'vstListImp.*')
    Left = 152
    Top = 8
    LangData = {
      0D0066726D496D706F72744C697374010100000001000000070043617074696F
      6E0111000000040062766C310000040062766C320000070062746E4261636B01
      0100000002000000070043617074696F6E00070062746E4E6578740101000000
      03000000070043617074696F6E00090062746E43616E63656C01010000000400
      0000070043617074696F6E000900706E6C486561646572000008006C626C5469
      746C65010100000005000000070043617074696F6E000900706763496D706F72
      7400000D00747341736B46696C654C6973740101000000140000000700436170
      74696F6E000600676246696C6501010000000D000000070043617074696F6E00
      07006C626C46696C6501010000000E000000070043617074696F6E0006007473
      4C69737400000A007673744C697374496D7000000C0062746E53656C65637441
      6C6C010100000012000000070043617074696F6E000E0062746E446573656C65
      6374416C6C010100000013000000070043617074696F6E000C00584D4C446F63
      756D656E743100000B00656474506174684C6973740000}
  end
end
