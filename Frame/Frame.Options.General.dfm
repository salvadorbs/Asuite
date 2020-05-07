inherited frmGeneralOptionsPage: TfrmGeneralOptionsPage
  object gbStartup: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 439
    Height = 110
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Startup'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object cbWindowsStartup: TCheckBox
      Left = 8
      Top = 18
      Width = 420
      Height = 17
      Caption = 'Start ASuite on Windows system startup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbShowPanelStartup: TCheckBox
      Left = 8
      Top = 52
      Width = 420
      Height = 17
      Caption = 'Show window on ASuite startup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object cbShowMenuStartup: TCheckBox
      Left = 8
      Top = 69
      Width = 420
      Height = 17
      Caption = 'Show graphic menu on ASuite startup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object chkMissedSchedulerTask: TCheckBox
      Left = 8
      Top = 35
      Width = 420
      Height = 17
      Caption = 'Check missed scheduler task at ASuite startup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object chkCheckUpdatesStartup: TCheckBox
      Left = 8
      Top = 86
      Width = 420
      Height = 17
      Caption = 'Check for updates on ASuite startup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object grpLanguage: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 244
    Width = 174
    Height = 47
    Margins.Bottom = 0
    Caption = 'Language'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object cxLanguage: TComboBox
      Left = 8
      Top = 17
      Width = 158
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object gbExecution: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 116
    Width = 439
    Height = 125
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Execution'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbActionOnExe: TLabel
      Left = 8
      Top = 53
      Width = 64
      Height = 13
      Caption = 'On execution'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object cbRunSingleClick: TCheckBox
      Left = 8
      Top = 18
      Width = 420
      Height = 17
      Caption = 'Execute with single click in main window'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cxActionOnExe: TComboBox
      Left = 8
      Top = 69
      Width = 169
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Items.Strings = (
        'Just run file'
        'Run and hide ASuite'
        'Run and close ASuite')
    end
    object chkConfirmMessageCat: TCheckBox
      Left = 8
      Top = 36
      Width = 420
      Height = 17
      Caption = 'Ask to confirm before run a category'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object chkAutoCloseProcess: TCheckBox
      Left = 8
      Top = 96
      Width = 420
      Height = 17
      Caption = 'Close all process opened by ASuite at launcher shutdown'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object grpTheme: TGroupBox
    AlignWithMargins = True
    Left = 183
    Top = 244
    Width = 174
    Height = 47
    Margins.Bottom = 0
    Caption = 'Theme'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    object cbTheme: TComboBox
      Left = 8
      Top = 17
      Width = 158
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Items.Strings = (
        'Windows settings'
        'Light'
        'Dark')
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 368
    Top = 344
    LangData = {
      150066726D47656E6572616C4F7074696F6E7350616765000110000000090067
      6253746172747570010100000005000000070043617074696F6E001000636257
      696E646F777353746172747570010100000006000000070043617074696F6E00
      1200636253686F7750616E656C53746172747570010100000007000000070043
      617074696F6E001100636253686F774D656E7553746172747570010100000008
      000000070043617074696F6E00160063686B4D69737365645363686564756C65
      725461736B010100000009000000070043617074696F6E000B006772704C616E
      6775616765010100000011000000070043617074696F6E000A0063784C616E67
      7561676500000B006762457865637574696F6E01010000001200000007004361
      7074696F6E000D006C62416374696F6E4F6E4578650101000000130000000700
      43617074696F6E001000636252756E53696E676C65436C69636B010100000014
      000000070043617074696F6E000D006378416374696F6E4F6E45786501010000
      001500000005004974656D7300140063686B436F6E6669726D4D657373616765
      436174010100000016000000070043617074696F6E00130063686B4175746F43
      6C6F736550726F63657373010100000017000000070043617074696F6E001600
      63686B436865636B557064617465735374617274757001010000001800000007
      0043617074696F6E0008006772705468656D6501010000001900000007004361
      7074696F6E00070063625468656D6501010000001A00000005004974656D7300}
  end
end
