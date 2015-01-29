inherited frmAdvancedOptionsPage: TfrmAdvancedOptionsPage
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 254
    Height = 380
    Align = alLeft
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object gbMFU: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 78
      Width = 248
      Height = 73
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Most Frequently Used'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      ExplicitTop = 152
      object lbMaxMFU: TLabel
        Left = 8
        Top = 43
        Width = 111
        Height = 13
        Caption = 'Max number MFU items'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object cbMFU: TCheckBox
        Left = 8
        Top = 18
        Width = 243
        Height = 19
        Caption = 'Active Most Frequently Used items'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = cbMFUClick
      end
      object seMFU: TSpinEdit
        Left = 185
        Top = 39
        Width = 57
        Height = 22
        MaxValue = 20
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
    end
    object gbRecents: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 248
      Height = 72
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Recents'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object lbMaxMRU: TLabel
        Left = 8
        Top = 43
        Width = 121
        Height = 13
        Caption = 'Max number recent items'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object cbMRU: TCheckBox
        Left = 8
        Top = 18
        Width = 241
        Height = 19
        Caption = 'Active recent items'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = cbMRUClick
      end
      object seRecents: TSpinEdit
        Left = 185
        Top = 39
        Width = 57
        Height = 22
        MaxValue = 20
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
    end
    object gbBackup: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 154
      Width = 248
      Height = 72
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Backup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      ExplicitLeft = 0
      ExplicitTop = 238
      object lbMaxBackup: TLabel
        Left = 8
        Top = 43
        Width = 118
        Height = 13
        Caption = 'Max number backup files'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object cbBackup: TCheckBox
        Left = 8
        Top = 20
        Width = 237
        Height = 19
        Caption = 'Active automatic backup'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = cbBackupClick
      end
      object seBackup: TSpinEdit
        Left = 185
        Top = 39
        Width = 57
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
    end
  end
  object pnlRight: TPanel
    Left = 254
    Top = 0
    Width = 191
    Height = 380
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object gbOtherFunctions: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 153
      Width = 185
      Height = 73
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Other functions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object cbCache: TCheckBox
        Left = 8
        Top = 17
        Width = 167
        Height = 19
        Caption = 'Enable cache'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object cbScheduler: TCheckBox
        Left = 8
        Top = 36
        Width = 167
        Height = 17
        Caption = 'Enable scheduler'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
    object grpClearElements: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 185
      Height = 147
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Clear elements'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object lbClearElements: TLabel
        Left = 8
        Top = 18
        Width = 135
        Height = 13
        Caption = 'Clear the following elements'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object cbClearMFU: TCheckBox
        Left = 8
        Top = 52
        Width = 169
        Height = 17
        Caption = 'Most Frequently Used'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = UpdateBtnClear
      end
      object cbClearBackup: TCheckBox
        Left = 8
        Top = 69
        Width = 169
        Height = 17
        Caption = 'Backups'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = UpdateBtnClear
      end
      object cbClearCache: TCheckBox
        Left = 8
        Top = 86
        Width = 169
        Height = 17
        Caption = 'Cache icons'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = UpdateBtnClear
      end
      object btnClear: TButton
        Left = 52
        Top = 109
        Width = 82
        Height = 23
        Caption = 'Clear'
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = btnClearClick
      end
      object cbClearMRU: TCheckBox
        Left = 8
        Top = 34
        Width = 169
        Height = 17
        Caption = 'Recents'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = UpdateBtnClear
      end
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 248
    LangData = {
      160066726D416476616E6365644F7074696F6E73506167650001180000000700
      706E6C4C6566740000050067624D465501010000000400000007004361707469
      6F6E0008006C624D61784D4655010100000005000000070043617074696F6E00
      050063624D4655010100000006000000070043617074696F6E00090067625265
      63656E7473010100000007000000070043617074696F6E0008006C624D61784D
      5255010100000008000000070043617074696F6E00050063624D525501010000
      0009000000070043617074696F6E000800706E6C52696768740000100067624F
      7468657246756E6374696F6E7301010000000A000000070043617074696F6E00
      07006362436163686501010000000B000000070043617074696F6E000B006362
      5363686564756C657201010000000C000000070043617074696F6E0010006772
      70436C656172456C656D656E747301010000000D000000070043617074696F6E
      000F006C62436C656172456C656D656E747301010000000E0000000700436170
      74696F6E000A006362436C6561724D4655010100000010000000070043617074
      696F6E000D006362436C6561724261636B757001010000001100000007004361
      7074696F6E000C006362436C6561724361636865010100000012000000070043
      617074696F6E00080062746E436C656172010100000013000000070043617074
      696F6E000A006362436C6561724D525501010000001400000007004361707469
      6F6E0009007365526563656E74730000050073654D4655000008006762426163
      6B7570010100000015000000070043617074696F6E000B006C624D6178426163
      6B7570010100000016000000070043617074696F6E00080063624261636B7570
      010100000017000000070043617074696F6E00080073654261636B75700000}
  end
end
