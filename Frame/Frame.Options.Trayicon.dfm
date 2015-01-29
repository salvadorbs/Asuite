inherited frmTrayiconOptionsPage: TfrmTrayiconOptionsPage
  object gbTrayicon: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 439
    Height = 134
    Margins.Bottom = 0
    Align = alTop
    Caption = 'System Tray'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lbTrayLeftClick: TLabel
      Left = 8
      Top = 82
      Width = 41
      Height = 13
      Caption = 'Left click'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbTrayRightClick: TLabel
      Left = 292
      Top = 82
      Width = 47
      Height = 13
      Caption = 'Right click'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblMiddleClick: TLabel
      Left = 152
      Top = 82
      Width = 52
      Height = 13
      Caption = 'Middle click'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object cxLeftClick: TComboBox
      Left = 8
      Top = 98
      Width = 138
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Items.Strings = (
        'No action'
        'Show window'
        'Show graphic menu'
        'Show classic menu')
    end
    object cbTrayicon: TCheckBox
      Left = 8
      Top = 20
      Width = 304
      Height = 19
      Caption = 'Enable the System Tray icon'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = cbTrayiconClick
    end
    object cxRightClick: TComboBox
      Left = 292
      Top = 98
      Width = 138
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      Items.Strings = (
        'No action'
        'Show window'
        'Show graphic menu'
        'Show classic menu')
    end
    object cbTrayCustomIcon: TCheckBox
      Left = 8
      Top = 39
      Width = 304
      Height = 19
      Caption = 'Custom tray icon'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = cbTrayCustomIconClick
    end
    object edtCustomIcon: TJvFilenameEdit
      Left = 8
      Top = 58
      Width = 422
      Height = 21
      OnBeforeDialog = edtCustomIconBeforeDialog
      OnAfterDialog = edtCustomIconAfterDialog
      AddQuotes = False
      DialogOptions = [ofHideReadOnly, ofEnableSizing]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = '$ASuite\'
    end
    object cxMiddleClick: TComboBox
      Left = 150
      Top = 98
      Width = 138
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      Items.Strings = (
        'No action'
        'Show window'
        'Show graphic menu'
        'Show classic menu')
    end
  end
  object grpClassicMenu: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 244
    Width = 439
    Height = 83
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Classic Menu'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object cbSubMenuMFU: TCheckBox
      Left = 3
      Top = 18
      Width = 310
      Height = 19
      Caption = 'Most Used items on trayicon submenu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbSubMenuMRU: TCheckBox
      Left = 3
      Top = 37
      Width = 308
      Height = 19
      Caption = 'Recent items on trayicon submenu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object chkAutoExpansion: TCheckBox
      Left = 3
      Top = 56
      Width = 308
      Height = 19
      Caption = 'Active autoexpansion folder'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object grpGraphicMenu: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 140
    Width = 439
    Height = 101
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Graphic Menu'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbMenuTheme: TLabel
      Left = 8
      Top = 53
      Width = 32
      Height = 13
      Caption = 'Theme'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cxTheme: TComboBox
      Left = 8
      Top = 66
      Width = 145
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object cbMenuFade: TCheckBox
      Left = 8
      Top = 17
      Width = 214
      Height = 19
      Caption = 'Enable fade effect'
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
      Top = 36
      Width = 214
      Height = 17
      Caption = 'Small TreeView'#39's icon size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object chkAutomaticHideMenu: TCheckBox
      Left = 228
      Top = 36
      Width = 209
      Height = 17
      Caption = 'Auto-hide menu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object chkUserPicture: TCheckBox
      Left = 228
      Top = 17
      Width = 209
      Height = 19
      Caption = 'Show user picture in menu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 376
    Top = 280
    LangData = {
      160066726D5472617969636F6E4F7074696F6E73506167650001150000000A00
      67625472617969636F6E010100000001000000070043617074696F6E000F006C
      62547261794C656674436C69636B010100000002000000070043617074696F6E
      0010006C62547261795269676874436C69636B01010000000300000007004361
      7074696F6E000E006C626C4D6964646C65436C69636B01010000000400000007
      0043617074696F6E000B0063784C656674436C69636B01010000000500000005
      004974656D73000A0063625472617969636F6E01010000000600000007004361
      7074696F6E000C0063785269676874436C69636B010100000007000000050049
      74656D73001000636254726179437573746F6D49636F6E010100000008000000
      070043617074696F6E000D00656474437573746F6D49636F6E01010000000900
      0000040054657874000D0063784D6964646C65436C69636B01010000000A0000
      0005004974656D73000E00677270436C61737369634D656E7501010000000B00
      0000070043617074696F6E000C0063625375624D656E754D465501010000000C
      000000070043617074696F6E000C0063625375624D656E754D52550101000000
      0D000000070043617074696F6E00100063686B4175746F457870616E73696F6E
      01010000000E000000070043617074696F6E000E00677270477261706869634D
      656E7501010000000F000000070043617074696F6E000B006C624D656E755468
      656D65010100000010000000070043617074696F6E00070063785468656D6500
      000A0063624D656E7546616465010100000011000000070043617074696F6E00
      0B006362536D616C6C49636F6E010100000012000000070043617074696F6E00
      140063686B4175746F6D61746963486964654D656E7501010000001300000007
      0043617074696F6E000E0063686B557365725069637475726501010000001400
      0000070043617074696F6E00}
  end
end
