inherited frmHotkeyOptionsPage: TfrmHotkeyOptionsPage
  object gbHotkey: TGroupBox [0]
    Left = 3
    Top = 0
    Width = 320
    Height = 129
    Caption = 'Hotkey'
    TabOrder = 0
    object cxWindowHotKeyCode: TComboBox
      Left = 167
      Top = 52
      Width = 57
      Height = 21
      Style = csDropDownList
      TabOrder = 3
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
    object cxWindowHotKeyMod: TComboBox
      Left = 8
      Top = 52
      Width = 153
      Height = 21
      Style = csDropDownList
      TabOrder = 2
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
    object cbWindowHotKey: TCheckBox
      Left = 8
      Top = 32
      Width = 233
      Height = 17
      Caption = 'Show window when hotkey is pressed'
      TabOrder = 1
      OnClick = cbWindowHotKeyClick
    end
    object cbMenuHotKey: TCheckBox
      Left = 8
      Top = 79
      Width = 241
      Height = 17
      Caption = 'Show trayicon menu when hotkey is pressed'
      TabOrder = 4
      OnClick = cbMenuHotKeyClick
    end
    object cxMenuHotKeyCode: TComboBox
      Left = 167
      Top = 99
      Width = 57
      Height = 21
      Style = csDropDownList
      TabOrder = 6
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
    object cxMenuHotKeyMod: TComboBox
      Left = 8
      Top = 99
      Width = 153
      Height = 21
      Style = csDropDownList
      TabOrder = 5
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
    object cbHotKey: TCheckBox
      Left = 8
      Top = 16
      Width = 233
      Height = 17
      Caption = 'Enable hotkey'
      TabOrder = 0
      OnClick = cbHotKeyClick
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Items')
    Left = 8
    Top = 248
    LangData = {
      140066726D486F746B65794F7074696F6E73506167650001090000000B004F70
      656E4469616C6F6731000008006762486F746B65790101000000010000000700
      43617074696F6E001200637857696E646F77486F744B6579436F646500001100
      637857696E646F77486F744B65794D6F6400000E00636257696E646F77486F74
      4B6579010100000004000000070043617074696F6E000C0063624D656E75486F
      744B6579010100000005000000070043617074696F6E00100063784D656E7548
      6F744B6579436F646500000F0063784D656E75486F744B65794D6F6400000800
      6362486F744B6579010100000008000000070043617074696F6E00}
  end
end
