inherited frmHotkeyOptionsPage: TfrmHotkeyOptionsPage
  object cbHotKey: TCheckBox
    Left = 8
    Top = 16
    Width = 233
    Height = 17
    Caption = 'Enable hotkey'
    TabOrder = 0
  end
  object gbWindow: TGroupBox
    Left = 8
    Top = 55
    Width = 257
    Height = 265
    Caption = 'Window'
    TabOrder = 1
    object cxWindowHotKeyCode: TComboBox
      Left = 167
      Top = 84
      Width = 57
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
    object cxWindowHotKeyMod: TComboBox
      Left = 8
      Top = 84
      Width = 153
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
    object cbWindowHotKey: TCheckBox
      Left = 8
      Top = 64
      Width = 233
      Height = 17
      Caption = 'Show window when hotkey is pressed'
      TabOrder = 0
    end
    object cbMenuHotKey: TCheckBox
      Left = 8
      Top = 111
      Width = 241
      Height = 17
      Caption = 'Show trayicon menu when hotkey is pressed'
      TabOrder = 3
    end
    object cxMenuHotKeyCode: TComboBox
      Left = 167
      Top = 131
      Width = 57
      Height = 21
      Style = csDropDownList
      TabOrder = 5
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
      Top = 131
      Width = 153
      Height = 21
      Style = csDropDownList
      TabOrder = 4
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
  end
end
