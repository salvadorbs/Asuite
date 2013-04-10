object frmStats: TfrmStats
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Stats'
  ClientHeight = 241
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbSystem: TGroupBox
    Left = 8
    Top = 8
    Width = 209
    Height = 65
    Caption = 'System'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lbOs: TLabel
      Left = 8
      Top = 16
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
    end
    object lbNamePc: TLabel
      Left = 8
      Top = 32
      Width = 74
      Height = 13
      Caption = 'Computer name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbUser: TLabel
      Left = 8
      Top = 48
      Width = 57
      Height = 13
      Caption = 'Current user'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbOs2: TLabel
      Left = 104
      Top = 16
      Width = 92
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Windows XP (SP2)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbNamePc2: TLabel
      Left = 104
      Top = 48
      Width = 91
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'User Xyz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbUser2: TLabel
      Left = 104
      Top = 32
      Width = 91
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Computer Xyz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object gbSupport: TGroupBox
    Left = 8
    Top = 80
    Width = 209
    Height = 65
    Caption = 'Drive X:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbSize: TLabel
      Left = 8
      Top = 16
      Width = 20
      Height = 13
      Caption = 'Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSpaceUsed: TLabel
      Left = 8
      Top = 32
      Width = 53
      Height = 13
      Caption = 'Free space'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSpaceFree: TLabel
      Left = 8
      Top = 48
      Width = 57
      Height = 13
      Caption = 'Used space'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSpaceFree2: TLabel
      Left = 104
      Top = 32
      Width = 91
      Height = 13
      Alignment = taRightJustify
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
      Left = 104
      Top = 16
      Width = 91
      Height = 13
      Alignment = taRightJustify
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
      Left = 104
      Top = 48
      Width = 91
      Height = 13
      Alignment = taRightJustify
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
  object gbASuite: TGroupBox
    Left = 8
    Top = 152
    Width = 209
    Height = 81
    Caption = 'ASuite'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lbSoftware: TLabel
      Left = 8
      Top = 16
      Width = 75
      Height = 13
      Caption = 'Software added'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbCat: TLabel
      Left = 8
      Top = 48
      Width = 83
      Height = 13
      Caption = 'Categories added'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbTotal: TLabel
      Left = 8
      Top = 64
      Width = 24
      Height = 13
      Caption = 'Total'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSoftwareGroup: TLabel
      Left = 8
      Top = 32
      Width = 105
      Height = 13
      Caption = 'Software group added'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSoftware2: TLabel
      Left = 104
      Top = 16
      Width = 91
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbSoftwareGroup2: TLabel
      Left = 120
      Top = 32
      Width = 75
      Height = 13
      Alignment = taRightJustify
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
      Left = 104
      Top = 48
      Width = 91
      Height = 13
      Alignment = taRightJustify
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
      Left = 104
      Top = 64
      Width = 91
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
end
