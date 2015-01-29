inherited frmStatsOptionsPage: TfrmStatsOptionsPage
  object gbASuite: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 222
    Width = 439
    Height = 83
    Margins.Bottom = 0
    Align = alTop
    Caption = 'ASuite'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lbSoftware: TLabel
      Left = 12
      Top = 24
      Width = 75
      Height = 13
      Caption = 'Software added'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbCat: TLabel
      Left = 12
      Top = 40
      Width = 83
      Height = 13
      Caption = 'Categories added'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbTotal: TLabel
      Left = 11
      Top = 63
      Width = 30
      Height = 13
      Caption = 'Total'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbSoftware2: TLabel
      Left = 223
      Top = 24
      Width = 209
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbCat2: TLabel
      Left = 223
      Top = 40
      Width = 209
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbTotal2: TLabel
      Left = 222
      Top = 63
      Width = 209
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object gbSupport: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 140
    Width = 439
    Height = 79
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Drive %S:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbSize: TLabel
      Left = 12
      Top = 24
      Width = 20
      Height = 13
      Caption = 'Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbSpaceUsed: TLabel
      Left = 12
      Top = 40
      Width = 53
      Height = 13
      Caption = 'Free space'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbSpaceFree: TLabel
      Left = 12
      Top = 56
      Width = 57
      Height = 13
      Caption = 'Used space'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbSpaceFree2: TLabel
      Left = 223
      Top = 40
      Width = 209
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '00 GB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSize2: TLabel
      Left = 223
      Top = 24
      Width = 209
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '000 GB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSpaceUsed2: TLabel
      Left = 223
      Top = 56
      Width = 209
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '00 GB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object gbSystem: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 439
    Height = 134
    Margins.Bottom = 0
    Align = alTop
    Caption = 'System'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lbOs: TLabel
      Left = 12
      Top = 24
      Width = 89
      Height = 13
      AutoSize = False
      Caption = 'Operating System'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbNamePc: TLabel
      Left = 12
      Top = 92
      Width = 74
      Height = 13
      Caption = 'Computer name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbUser: TLabel
      Left = 12
      Top = 109
      Width = 57
      Height = 13
      Caption = 'Current user'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbOs2: TLabel
      Left = 128
      Top = 24
      Width = 304
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'Windows XP (SP2)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbUser2: TLabel
      Left = 128
      Top = 109
      Width = 304
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'User Xyz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbNamePc2: TLabel
      Left = 128
      Top = 92
      Width = 304
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'Computer Xyz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblBuild2: TLabel
      Left = 152
      Top = 41
      Width = 280
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '5.0 1234'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblBuild: TLabel
      Left = 12
      Top = 41
      Width = 89
      Height = 13
      AutoSize = False
      Caption = 'Build Version'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lblProcessor: TLabel
      Left = 12
      Top = 58
      Width = 47
      Height = 13
      Caption = 'Processor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lblProcessor2: TLabel
      Left = 128
      Top = 58
      Width = 303
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'Intel Pentium 4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblRam: TLabel
      Left = 12
      Top = 75
      Width = 104
      Height = 13
      Caption = 'Total physical memory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lblRam2: TLabel
      Left = 222
      Top = 75
      Width = 209
      Height = 13
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '4,00 GB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      'lbNamePc2.*'
      'lbOs2.*'
      'lbSize2.*'
      'lbSpaceFree2.*'
      'lbSpaceUsed2.*'
      'lbUser2.*')
    Left = 88
    Top = 320
    LangData = {
      130066726D53746174734F7074696F6E735061676500011B0000000A006C6253
      6F667477617265010100000002000000070043617074696F6E0005006C624361
      74010100000003000000070043617074696F6E0007006C62546F74616C010100
      000004000000070043617074696F6E000B006C62536F66747761726532000006
      006C6243617432000008006C62546F74616C32000009006762537570706F7274
      010100000005000000070043617074696F6E0006006C6253697A650101000000
      06000000070043617074696F6E000B006C625370616365557365640101000000
      07000000070043617074696F6E000B006C625370616365467265650101000000
      08000000070043617074696F6E000C006C625370616365467265653200000700
      6C6253697A653200000C006C6253706163655573656432000008006762537973
      74656D01010000000C000000070043617074696F6E0004006C624F7301010000
      000D000000070043617074696F6E0008006C624E616D65506301010000000E00
      0000070043617074696F6E0006006C625573657201010000000F000000070043
      617074696F6E0005006C624F7332000007006C625573657232000009006C624E
      616D655063320000080067624153756974650101000000100000000700436170
      74696F6E0009006C626C4275696C6432000008006C626C4275696C6401010000
      0012000000070043617074696F6E000C006C626C50726F636573736F72010100
      000013000000070043617074696F6E000D006C626C50726F636573736F723201
      0100000014000000070043617074696F6E0006006C626C52616D010100000015
      000000070043617074696F6E0007006C626C52616D3201010000001600000007
      0043617074696F6E00}
  end
end
