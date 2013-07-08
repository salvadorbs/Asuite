inherited frmGeneralOptionsPage: TfrmGeneralOptionsPage
  object gbWindow: TGroupBox
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
  object gbStartup: TGroupBox
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
  object gbTreeView: TGroupBox
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
  object grpLanguage: TGroupBox
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
end
