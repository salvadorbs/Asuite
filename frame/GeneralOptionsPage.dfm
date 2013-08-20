inherited frmGeneralOptionsPage: TfrmGeneralOptionsPage
  object gbWindow: TGroupBox [0]
    Left = 3
    Top = 3
    Width = 142
    Height = 134
    Caption = 'Window'
    TabOrder = 0
    object cbWindowOnTop: TCheckBox
      Left = 8
      Top = 36
      Width = 134
      Height = 17
      Caption = 'Window on top'
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
      Width = 134
      Height = 17
      Caption = 'Hold window size'
      TabOrder = 0
    end
    object cbCustomTitle: TCheckBox
      Left = 8
      Top = 85
      Width = 134
      Height = 17
      Caption = 'Custom window title'
      TabOrder = 3
      OnClick = cbCustomTitleClick
    end
    object edtCustomTitle: TEdit
      Left = 8
      Top = 102
      Width = 128
      Height = 21
      TabOrder = 4
      Text = 'ASuite'
    end
    object cbHideSearch: TCheckBox
      Left = 8
      Top = 53
      Width = 134
      Height = 17
      Caption = 'Hide tab Search'
      TabOrder = 2
    end
  end
  object gbStartup: TGroupBox [1]
    Left = 151
    Top = 3
    Width = 174
    Height = 81
    Caption = 'Startup'
    TabOrder = 1
    object cbWindowsStartup: TCheckBox
      Left = 8
      Top = 18
      Width = 168
      Height = 19
      Caption = 'Start ASuite on system startup'
      TabOrder = 0
    end
    object cbShowPanelStartup: TCheckBox
      Left = 8
      Top = 37
      Width = 168
      Height = 19
      Caption = 'Show window on startup'
      TabOrder = 1
    end
    object cbShowMenuStartup: TCheckBox
      Left = 8
      Top = 56
      Width = 168
      Height = 19
      Caption = 'Show Menu on startup'
      TabOrder = 2
    end
  end
  object gbTreeView: TGroupBox [2]
    Left = 3
    Top = 137
    Width = 324
    Height = 114
    Caption = 'Treeview'
    TabOrder = 2
    object cbBackground: TCheckBox
      Left = 8
      Top = 32
      Width = 304
      Height = 19
      Caption = 'Active background'
      TabOrder = 1
      OnClick = cbBackgroundClick
    end
    object btnFontSettings: TButton
      Left = 101
      Top = 79
      Width = 121
      Height = 26
      Caption = 'Font settings...'
      TabOrder = 4
      OnClick = btnFontSettingsClick
    end
    object edtBackground: TEdit
      Left = 8
      Top = 52
      Width = 253
      Height = 21
      TabOrder = 2
      Text = '$ASuite\'
    end
    object btnBrowseBackground: TButton
      Left = 265
      Top = 52
      Width = 53
      Height = 21
      Caption = 'Browse'
      TabOrder = 3
      OnClick = btnBrowseBackgroundClick
    end
    object cbAutoOpClCat: TCheckBox
      Left = 8
      Top = 16
      Width = 304
      Height = 19
      Caption = 'Automatic Opening/Closing Categories'
      TabOrder = 0
    end
  end
  object grpLanguage: TGroupBox [3]
    Left = 151
    Top = 84
    Width = 174
    Height = 53
    Caption = 'Language'
    TabOrder = 3
    object cxLanguage: TComboBox
      Left = 8
      Top = 21
      Width = 158
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 280
    Top = 232
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 8
    Top = 248
    LangData = {
      150066726D47656E6572616C4F7074696F6E73506167650001140000000B004F
      70656E4469616C6F673100000800676257696E646F7701010000000100000007
      0043617074696F6E000D00636257696E646F774F6E546F700101000000020000
      00070043617074696F6E000A006362486F6C6453697A65010100000003000000
      070043617074696F6E000D006362437573746F6D5469746C6501010000000400
      0000070043617074696F6E000E00656474437573746F6D5469746C6500000C00
      636248696465536561726368010100000006000000070043617074696F6E0009
      00676253746172747570010100000007000000070043617074696F6E00100063
      6257696E646F777353746172747570010100000008000000070043617074696F
      6E001200636253686F7750616E656C5374617274757001010000000900000007
      0043617074696F6E001100636253686F774D656E755374617274757001010000
      000A000000070043617074696F6E000A00676254726565566965770101000000
      0B000000070043617074696F6E000C0063624261636B67726F756E6401010000
      000C000000070043617074696F6E000F0062746E466F6E7453657474696E6773
      01010000000D000000070043617074696F6E000D006564744261636B67726F75
      6E640000130062746E42726F7773654261636B67726F756E6401010000000F00
      0000070043617074696F6E000D0063624175746F4F70436C4361740101000000
      10000000070043617074696F6E000B006772704C616E67756167650101000000
      11000000070043617074696F6E000A0063784C616E677561676500000B00466F
      6E744469616C6F67310000}
  end
end
