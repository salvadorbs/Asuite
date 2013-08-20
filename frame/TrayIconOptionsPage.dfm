inherited frmTrayiconOptionsPage: TfrmTrayiconOptionsPage
  object gbTrayicon: TGroupBox [0]
    Left = 3
    Top = 3
    Width = 320
    Height = 158
    Caption = 'System Tray'
    TabOrder = 0
    object lbTrayLeftClick: TLabel
      Left = 8
      Top = 106
      Width = 41
      Height = 13
      Caption = 'Left click'
      Color = clBtnFace
      ParentColor = False
    end
    object lbTrayRightClick: TLabel
      Left = 164
      Top = 106
      Width = 47
      Height = 13
      Caption = 'Right click'
      Color = clBtnFace
      ParentColor = False
    end
    object cxLeftClick: TComboBox
      Left = 8
      Top = 119
      Width = 150
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      Items.Strings = (
        'No action'
        'Show window'
        'Show menu')
    end
    object btnBrowse: TButton
      Left = 255
      Top = 58
      Width = 55
      Height = 21
      Caption = 'Browse'
      TabOrder = 3
      OnClick = btnBrowseClick
    end
    object cbTrayicon: TCheckBox
      Left = 8
      Top = 20
      Width = 156
      Height = 19
      Caption = 'Enable the System Tray icon'
      TabOrder = 0
      OnClick = cbTrayiconClick
    end
    object cxRightClick: TComboBox
      Left = 162
      Top = 119
      Width = 150
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      Items.Strings = (
        'No action'
        'Show window'
        'Show menu')
    end
    object cbTrayCustomIcon: TCheckBox
      Left = 8
      Top = 39
      Width = 101
      Height = 19
      Caption = 'Custom tray icon'
      TabOrder = 1
      OnClick = cbTrayCustomIconClick
    end
    object cbClassicMenu: TCheckBox
      Left = 8
      Top = 79
      Width = 302
      Height = 17
      Caption = 'Use classic menu'
      TabOrder = 4
      OnClick = cbClassicMenuClick
    end
    object edtCustomIcon: TEdit
      Left = 8
      Top = 58
      Width = 241
      Height = 21
      TabOrder = 2
    end
  end
  object grpGraphicMenu: TGroupBox [1]
    Left = 3
    Top = 167
    Width = 158
    Height = 101
    Caption = 'Graphic Menu'
    TabOrder = 1
    object lbMenuTheme: TLabel
      Left = 8
      Top = 42
      Width = 32
      Height = 13
      Caption = 'Theme'
    end
    object cxTheme: TComboBox
      Left = 8
      Top = 58
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object cbMenuFade: TCheckBox
      Left = 8
      Top = 17
      Width = 241
      Height = 19
      Caption = 'Enable fade menu'
      TabOrder = 0
    end
  end
  object grpClassicMenu: TGroupBox [2]
    Left = 165
    Top = 167
    Width = 158
    Height = 101
    Caption = 'Classic Menu'
    TabOrder = 2
    object cbSubMenuMFU: TCheckBox
      Left = 3
      Top = 18
      Width = 144
      Height = 19
      Caption = 'MFU on trayicon submenu'
      TabOrder = 0
    end
    object cbSubMenuMRU: TCheckBox
      Left = 3
      Top = 34
      Width = 145
      Height = 19
      Caption = 'MRU on trayicon submenu'
      TabOrder = 1
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 248
    LangData = {
      160066726D5472617969636F6E4F7074696F6E73506167650001120000000B00
      4F70656E4469616C6F673100000A0067625472617969636F6E01010000000100
      0000070043617074696F6E000F006C62547261794C656674436C69636B010100
      000002000000070043617074696F6E0010006C62547261795269676874436C69
      636B010100000003000000070043617074696F6E000B0063784C656674436C69
      636B01010000000400000005004974656D7300090062746E42726F7773650101
      00000005000000070043617074696F6E000A0063625472617969636F6E010100
      000006000000070043617074696F6E000C0063785269676874436C69636B0101
      0000000700000005004974656D73001000636254726179437573746F6D49636F
      6E010100000008000000070043617074696F6E000D006362436C61737369634D
      656E75010100000009000000070043617074696F6E000D00656474437573746F
      6D49636F6E00000E00677270477261706869634D656E7501010000000A000000
      070043617074696F6E000B006C624D656E755468656D6501010000000B000000
      070043617074696F6E00070063785468656D6500000A0063624D656E75466164
      6501010000000C000000070043617074696F6E000E00677270436C6173736963
      4D656E7501010000000D000000070043617074696F6E000C0063625375624D65
      6E754D465501010000000E000000070043617074696F6E000C0063625375624D
      656E754D525501010000000F000000070043617074696F6E00}
  end
end
