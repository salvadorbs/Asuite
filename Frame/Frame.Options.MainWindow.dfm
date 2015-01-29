inherited frmMainWindowOptionsPage: TfrmMainWindowOptionsPage
  object gbTreeView: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 126
    Width = 439
    Height = 103
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Treeview'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object cbBackground: TCheckBox
      Left = 8
      Top = 52
      Width = 295
      Height = 19
      Caption = 'Active custom background'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = cbBackgroundClick
    end
    object btnFontSettings: TButton
      Left = 309
      Top = 27
      Width = 121
      Height = 25
      Caption = 'Font settings...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnFontSettingsClick
    end
    object cbAutoOpClCat: TCheckBox
      Left = 8
      Top = 16
      Width = 295
      Height = 19
      Caption = 'Automatic Opening/Closing Categories'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbSmallIcon: TCheckBox
      Left = 8
      Top = 35
      Width = 295
      Height = 17
      Caption = 'Small icon size in main window'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object edtBackground: TJvFilenameEdit
      Left = 8
      Top = 71
      Width = 422
      Height = 21
      OnBeforeDialog = edtBackgroundBeforeDialog
      OnAfterDialog = edtBackgroundAfterDialog
      AddQuotes = False
      Flat = False
      ParentFlat = False
      DialogOptions = [ofHideReadOnly, ofEnableSizing]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ButtonWidth = 20
      ParentFont = False
      TabOrder = 4
      Text = '$ASuite\'
    end
  end
  object gbWindow: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 439
    Height = 120
    Margins.Bottom = 0
    Align = alTop
    Caption = 'General'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object cbWindowOnTop: TCheckBox
      Left = 8
      Top = 36
      Width = 420
      Height = 17
      Caption = 'Main window always on top'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cbHoldSize: TCheckBox
      Left = 8
      Top = 19
      Width = 420
      Height = 17
      Caption = 'Hold main window size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbHideSearch: TCheckBox
      Left = 8
      Top = 53
      Width = 420
      Height = 17
      Caption = 'Hide tab Search in main window'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object edtCustomTitle: TEdit
      Left = 18
      Top = 93
      Width = 128
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      Text = 'ASuite'
    end
    object cbCustomTitle: TCheckBox
      Left = 8
      Top = 70
      Width = 420
      Height = 17
      Caption = 'Custom main window title'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = cbCustomTitleClick
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 344
    Top = 256
    LangData = {
      180066726D4D61696E57696E646F774F7074696F6E735061676500010D000000
      0A0067625472656556696577010100000001000000070043617074696F6E000C
      0063624261636B67726F756E64010100000002000000070043617074696F6E00
      0F0062746E466F6E7453657474696E6773010100000003000000070043617074
      696F6E000D0063624175746F4F70436C43617401010000000400000007004361
      7074696F6E000B006362536D616C6C49636F6E01010000000500000007004361
      7074696F6E000D006564744261636B67726F756E6400000800676257696E646F
      77010100000006000000070043617074696F6E000D00636257696E646F774F6E
      546F70010100000007000000070043617074696F6E000A006362486F6C645369
      7A65010100000008000000070043617074696F6E000C00636248696465536561
      726368010100000009000000070043617074696F6E000E00656474437573746F
      6D5469746C6500000D006362437573746F6D5469746C6501010000000A000000
      070043617074696F6E000B00466F6E744469616C6F67310000}
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 246
    Top = 254
  end
end
