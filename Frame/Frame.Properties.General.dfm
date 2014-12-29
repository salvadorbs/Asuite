inherited frmBaseGeneralPropertyPage: TfrmBaseGeneralPropertyPage
  object gbItem: TGroupBox
    Left = 3
    Top = 3
    Width = 354
    Height = 86
    Caption = 'Item'
    TabOrder = 0
    object lbName: TLabel
      Left = 8
      Top = 11
      Width = 27
      Height = 13
      Caption = 'Name'
      Color = clBtnFace
      ParentColor = False
    end
    object lbPathIcon: TLabel
      Left = 8
      Top = 45
      Width = 132
      Height = 13
      Caption = 'Custom icon path (optional)'
      Color = clBtnFace
      ParentColor = False
    end
    object edtName: TEdit
      Left = 8
      Top = 24
      Width = 145
      Height = 21
      TabOrder = 0
      OnEnter = edtNameEnter
    end
    object edtPathIcon: TEdit
      Left = 8
      Top = 58
      Width = 267
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '$ASuite\'
    end
    object btnBrowseIcon: TButton
      Left = 281
      Top = 58
      Width = 65
      Height = 21
      Caption = 'Browse'
      TabOrder = 2
    end
  end
end
