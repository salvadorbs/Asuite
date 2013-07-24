inherited frmAdvancedPropertyPage: TfrmAdvancedPropertyPage
  Width = 360
  ExplicitWidth = 360
  object cbShortcutDesktop: TCheckBox [0]
    Left = 13
    Top = 195
    Width = 344
    Height = 19
    Caption = 'Create shortcut on desktop when ASuite is running'
    TabOrder = 3
  end
  object grpScheduler: TGroupBox [1]
    Left = 3
    Top = 3
    Width = 150
    Height = 105
    Caption = 'Scheduler'
    TabOrder = 0
    object cxScheduler: TComboBox
      Left = 10
      Top = 21
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 0
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
      Date = 39092.942071932870000000
      Time = 39092.942071932870000000
      TabOrder = 1
    end
    object dtpSchTime: TDateTimePicker
      Left = 10
      Top = 75
      Width = 129
      Height = 21
      Date = 39092.942361111110000000
      Format = 'HH:mm'
      Time = 39092.942361111110000000
      DateMode = dmUpDown
      Kind = dtkTime
      TabOrder = 2
    end
  end
  object grpHotkey: TGroupBox [2]
    Left = 159
    Top = 3
    Width = 198
    Height = 76
    Caption = 'Hotkey'
    TabOrder = 1
    object cbHotKey: TCheckBox
      Left = 8
      Top = 21
      Width = 162
      Height = 17
      Caption = 'Active hotkey'
      TabOrder = 0
    end
    object cxHotkey1: TComboBox
      Left = 8
      Top = 44
      Width = 130
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        'Alt'
        'Crtl'
        'Shift'
        'Crtl + Alt'
        'Shift + Alt'
        'Shift + Crtl'
        'Shift + Crtl + Alt'
        'WinKey'
        'WinKey + Alt'
        'WinKey + Crtl'
        'WinKey + Shift'
        'WinKey + Crtl + Alt'
        'WinKey + Shift + Alt'
        'WinKey + Shift + Crtl'
        'WinKey + Shift + Crtl + Alt')
    end
    object cxHotKey2: TComboBox
      Left = 140
      Top = 44
      Width = 49
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'A'
        'B'
        'C'
        'D'
        'E'
        'F'
        'G'
        'H'
        'I'
        'J'
        'K'
        'L'
        'M'
        'N'
        'O'
        'P'
        'Q'
        'R'
        'S'
        'T'
        'U'
        'V'
        'W'
        'X'
        'Y'
        'Z'
        'F1'
        'F2'
        'F3'
        'F4'
        'F5'
        'F6'
        'F7'
        'F8'
        'F9'
        'F10'
        'F11'
        'F12'
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9'
        '0')
    end
  end
  object GroupBox1: TGroupBox [3]
    Left = 3
    Top = 108
    Width = 354
    Height = 81
    Caption = 'Menu'
    TabOrder = 2
    object cbHideSoftware: TCheckBox
      Left = 8
      Top = 18
      Width = 307
      Height = 19
      Caption = 'Hide this software from menu'
      TabOrder = 0
    end
    object cbDontInsertMRU: TCheckBox
      Left = 8
      Top = 37
      Width = 307
      Height = 19
      Caption = 'Don'#39't insert this software in recents (MRU)'
      TabOrder = 1
    end
    object cbDontInsertMFU: TCheckBox
      Left = 8
      Top = 56
      Width = 307
      Height = 19
      Caption = 'Don'#39't insert this software in MFU'
      TabOrder = 2
    end
  end
end
