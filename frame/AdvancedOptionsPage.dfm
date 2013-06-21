inherited frmAdvancedOptionsPage: TfrmAdvancedOptionsPage
  object gbRecents: TGroupBox
    Left = 5
    Top = 3
    Width = 158
    Height = 94
    Caption = 'Recents'
    TabOrder = 0
    object lbMaxMRU: TLabel
      Left = 11
      Top = 37
      Width = 84
      Height = 13
      Caption = 'Max number MRU'
      Color = clBtnFace
      ParentColor = False
    end
    object lbNumbMRU: TLabel
      Left = 106
      Top = 57
      Width = 12
      Height = 13
      Caption = '10'
      Color = clBtnFace
      ParentColor = False
    end
    object cbMRU: TCheckBox
      Left = 8
      Top = 18
      Width = 147
      Height = 19
      Caption = 'Active MRU'
      TabOrder = 0
    end
    object tbMRU: TTrackBar
      Left = 3
      Top = 53
      Width = 97
      Height = 25
      ShowSelRange = False
      TabOrder = 1
    end
  end
  object gbMFU: TGroupBox
    Left = 167
    Top = 3
    Width = 158
    Height = 94
    Caption = 'Most Frequently Used'
    TabOrder = 1
    object lbMaxMFU: TLabel
      Left = 8
      Top = 37
      Width = 83
      Height = 13
      Caption = 'Max number MFU'
      Color = clBtnFace
      ParentColor = False
    end
    object lbNumbMFU: TLabel
      Left = 106
      Top = 57
      Width = 12
      Height = 13
      Caption = '10'
      Color = clBtnFace
      ParentColor = False
    end
    object cbMFU: TCheckBox
      Left = 8
      Top = 18
      Width = 74
      Height = 19
      Caption = 'Active MFU'
      TabOrder = 0
    end
    object tbMFU: TTrackBar
      Left = 8
      Top = 53
      Width = 97
      Height = 25
      ShowSelRange = False
      TabOrder = 1
    end
  end
  object gbBackup: TGroupBox
    Left = 5
    Top = 97
    Width = 158
    Height = 90
    Caption = 'Backup'
    TabOrder = 2
    object lbMaxBackup: TLabel
      Left = 8
      Top = 39
      Width = 118
      Height = 13
      Caption = 'Max number backup files'
      Color = clBtnFace
      ParentColor = False
    end
    object lbNumbBackup: TLabel
      Left = 106
      Top = 55
      Width = 12
      Height = 13
      Caption = '10'
      Color = clBtnFace
      ParentColor = False
    end
    object cbBackup: TCheckBox
      Left = 8
      Top = 20
      Width = 147
      Height = 19
      Caption = 'Active backup'
      TabOrder = 0
    end
    object tbBackup: TTrackBar
      Left = 3
      Top = 55
      Width = 97
      Height = 25
      ShowSelRange = False
      TabOrder = 1
    end
  end
  object grpClearElements: TGroupBox
    Left = 167
    Top = 97
    Width = 158
    Height = 118
    Caption = 'Clear elements'
    TabOrder = 3
    object lbClearElements: TLabel
      Left = 8
      Top = 16
      Width = 135
      Height = 13
      Caption = 'Clear the following elements'
    end
    object cbRecents: TCheckBox
      Left = 532
      Top = 72
      Width = 181
      Height = 17
      Caption = 'Recents'
      TabOrder = 0
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 29
      Width = 147
      Height = 17
      Caption = 'Most Frequently Used'
      TabOrder = 1
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 46
      Width = 147
      Height = 17
      Caption = 'Backups'
      TabOrder = 2
    end
    object cbCache: TCheckBox
      Left = 8
      Top = 63
      Width = 147
      Height = 17
      Caption = 'Cache icons'
      TabOrder = 3
    end
    object btnClear: TButton
      Left = 50
      Top = 86
      Width = 59
      Height = 25
      Caption = 'Clear'
      Default = True
      TabOrder = 4
    end
  end
  object gbOtherFunctions: TGroupBox
    Left = 5
    Top = 187
    Width = 158
    Height = 62
    Caption = 'Other functions'
    TabOrder = 4
    object CheckBox3: TCheckBox
      Left = 8
      Top = 17
      Width = 147
      Height = 19
      Caption = 'Enable cache'
      TabOrder = 0
    end
    object cbScheduler: TCheckBox
      Left = 8
      Top = 36
      Width = 147
      Height = 17
      Caption = 'Enable scheduler'
      TabOrder = 1
    end
  end
end
