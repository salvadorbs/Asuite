inherited frmAdvancedPropertyPage: TfrmAdvancedPropertyPage
  object cbShortcutDesktop: TCheckBox
    Left = 13
    Top = 195
    Width = 344
    Height = 19
    Caption = 'Create shortcut on desktop when ASuite is running'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object grpScheduler: TGroupBox
    Left = 3
    Top = 3
    Width = 150
    Height = 105
    Caption = 'Scheduler'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object cxScheduler: TComboBox
      Left = 10
      Top = 21
      Width = 129
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = cxSchedulerChange
      Items.Strings = (
        'Disabled'
        'Once'
        'Hourly'
        'Daily')
    end
    object dtpSchDate: TDateTimePicker
      Left = 10
      Top = 48
      Width = 129
      Height = 21
      Date = 39092.000000000000000000
      Time = 0.942071932870021600
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object dtpSchTime: TDateTimePicker
      Left = 10
      Top = 75
      Width = 129
      Height = 21
      Date = 39092.000000000000000000
      Format = 'HH:mm'
      Time = 0.942361111112404600
      DateMode = dmUpDown
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = dtkTime
      ParentFont = False
      TabOrder = 2
    end
  end
  object grpHotkey: TGroupBox
    Left = 159
    Top = 3
    Width = 198
    Height = 76
    Caption = 'Hotkey'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object cbHotKey: TCheckBox
      Left = 8
      Top = 21
      Width = 162
      Height = 17
      Caption = 'Active hotkey'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = cbHotKeyClick
    end
    object edtHotkey: TButtonedEdit
      Left = 8
      Top = 44
      Width = 187
      Height = 21
      CharCase = ecUpperCase
      LeftButton.Enabled = False
      ReadOnly = True
      TabOrder = 1
      OnChange = edtHotkeyChange
      OnClick = edtHotkeyClick
      OnRightButtonClick = edtHotkeyRightButtonClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 3
    Top = 108
    Width = 354
    Height = 81
    Caption = 'Menu'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object cbHideSoftware: TCheckBox
      Left = 8
      Top = 18
      Width = 307
      Height = 19
      Caption = 'Hide this software from menu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbDontInsertMRU: TCheckBox
      Left = 8
      Top = 37
      Width = 307
      Height = 19
      Caption = 'Don'#39't insert this software in recents (MRU)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cbDontInsertMFU: TCheckBox
      Left = 8
      Top = 56
      Width = 307
      Height = 19
      Caption = 'Don'#39't insert this software in MFU'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Format'
      'cxHotKeyCode.*'
      'cxHotkeyMod.*')
    Left = 8
    Top = 248
    LangData = {
      170066726D416476616E63656450726F70657274795061676500010C00000011
      00636253686F72746375744465736B746F700101000000010000000700436170
      74696F6E000C006772705363686564756C657201010000000200000007004361
      7074696F6E000B0063785363686564756C657201010000000300000005004974
      656D73000A006474705363684461746500000A0064747053636854696D650000
      0900677270486F746B6579010100000005000000070043617074696F6E000900
      47726F7570426F7831010100000009000000070043617074696F6E000E006362
      48696465536F66747761726501010000000A000000070043617074696F6E000F
      006362446F6E74496E736572744D525501010000000B00000007004361707469
      6F6E000F006362446F6E74496E736572744D465501010000000C000000070043
      617074696F6E0008006362486F744B657901010000000D000000070043617074
      696F6E000900656474486F746B65790000}
  end
end
