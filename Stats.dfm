inherited frmStats: TfrmStats
  Left = -283
  Top = 127
  Width = 225
  BorderStyle = bsDialog
  Caption = 'Stats'
  ClientWidth = 225
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  Position = poMainFormCenter
  object gbSystem: TGroupBox[0]
    Left = 8
    Height = 65
    Top = 8
    Width = 209
    Caption = 'System'
    ClientHeight = 47
    ClientWidth = 205
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lbOs: TLabel
      Left = 8
      Height = 13
      Top = 0
      Width = 89
      AutoSize = False
      Caption = 'Operating System'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbNamePc: TLabel
      Left = 8
      Height = 14
      Top = 16
      Width = 75
      Caption = 'Computer name'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbUser: TLabel
      Left = 8
      Height = 14
      Top = 32
      Width = 58
      Caption = 'Current user'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbOs2: TLabel
      Left = 104
      Height = 13
      Top = 0
      Width = 90
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Windows XP (SP2)'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbUser2: TLabel
      Left = 105
      Height = 13
      Top = 32
      Width = 90
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'UserXXXXX'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbNamePc2: TLabel
      Left = 105
      Height = 13
      Top = 16
      Width = 90
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'computerName'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
  end
  object gbSupport: TGroupBox[1]
    Left = 8
    Height = 65
    Top = 80
    Width = 209
    Caption = 'Drive %s:'
    ClientHeight = 47
    ClientWidth = 205
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbSize: TLabel
      Left = 8
      Height = 14
      Top = 0
      Width = 21
      Caption = 'Size'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSpaceUsed: TLabel
      Left = 8
      Height = 14
      Top = 16
      Width = 54
      Caption = 'Free space'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSpaceFree: TLabel
      Left = 8
      Height = 14
      Top = 32
      Width = 58
      Caption = 'Used space'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSpaceFree2: TLabel
      Left = 104
      Height = 13
      Top = 16
      Width = 91
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00 GB'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSize2: TLabel
      Left = 104
      Height = 13
      Top = 0
      Width = 91
      Alignment = taRightJustify
      AutoSize = False
      Caption = '000 GB'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSpaceUsed2: TLabel
      Left = 104
      Height = 13
      Top = 32
      Width = 91
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00 GB'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
  end
  object gbASuite: TGroupBox[2]
    Left = 8
    Height = 81
    Top = 152
    Width = 209
    Caption = 'ASuite'
    ClientHeight = 63
    ClientWidth = 205
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lbSoftware: TLabel
      Left = 8
      Height = 14
      Top = 0
      Width = 76
      Caption = 'Software added'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbCat: TLabel
      Left = 8
      Height = 14
      Top = 32
      Width = 84
      Caption = 'Categories added'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbTotal: TLabel
      Left = 8
      Height = 14
      Top = 48
      Width = 25
      Caption = 'Total'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSoftwareGroup: TLabel
      Left = 8
      Height = 14
      Top = 16
      Width = 106
      Caption = 'Software group added'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSoftware2: TLabel
      Left = 104
      Height = 13
      Top = 0
      Width = 91
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbSoftwareGroup2: TLabel
      Left = 120
      Height = 13
      Top = 16
      Width = 75
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbCat2: TLabel
      Left = 104
      Height = 13
      Top = 32
      Width = 91
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
    object lbTotal2: TLabel
      Left = 104
      Height = 13
      Top = 48
      Width = 91
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
    end
  end
end
