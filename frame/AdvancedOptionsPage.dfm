inherited frmAdvancedOptionsPage: TfrmAdvancedOptionsPage
  object gbRecents: TGroupBox [0]
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
      OnClick = cbMRUClick
    end
    object tbMRU: TTrackBar
      Left = 3
      Top = 53
      Width = 97
      Height = 25
      ShowSelRange = False
      TabOrder = 1
      OnChange = TrackBarChange
    end
  end
  object gbMFU: TGroupBox [1]
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
      OnClick = cbMFUClick
    end
    object tbMFU: TTrackBar
      Left = 8
      Top = 53
      Width = 97
      Height = 25
      ShowSelRange = False
      TabOrder = 1
      OnChange = TrackBarChange
    end
  end
  object gbBackup: TGroupBox [2]
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
      OnClick = cbBackupClick
    end
    object tbBackup: TTrackBar
      Left = 3
      Top = 55
      Width = 97
      Height = 25
      ShowSelRange = False
      TabOrder = 1
      OnChange = TrackBarChange
    end
  end
  object grpClearElements: TGroupBox [3]
    Left = 167
    Top = 97
    Width = 158
    Height = 152
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
    object cbClearMFU: TCheckBox
      Left = 8
      Top = 46
      Width = 147
      Height = 17
      Caption = 'Most Frequently Used'
      TabOrder = 2
      OnClick = UpdateBtnClear
    end
    object cbClearBackup: TCheckBox
      Left = 8
      Top = 63
      Width = 147
      Height = 17
      Caption = 'Backups'
      TabOrder = 3
      OnClick = UpdateBtnClear
    end
    object cbClearCache: TCheckBox
      Left = 8
      Top = 80
      Width = 147
      Height = 17
      Caption = 'Cache icons'
      TabOrder = 4
      OnClick = UpdateBtnClear
    end
    object btnClear: TButton
      Left = 49
      Top = 104
      Width = 59
      Height = 25
      Caption = 'Clear'
      Default = True
      TabOrder = 5
      OnClick = btnClearClick
    end
    object cbClearMRU: TCheckBox
      Left = 8
      Top = 29
      Width = 147
      Height = 17
      Caption = 'Recents'
      TabOrder = 1
      OnClick = UpdateBtnClear
    end
  end
  object gbOtherFunctions: TGroupBox [4]
    Left = 5
    Top = 187
    Width = 158
    Height = 62
    Caption = 'Other functions'
    TabOrder = 4
    object cbCache: TCheckBox
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
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 248
    LangData = {
      160066726D416476616E6365644F7074696F6E735061676500011B0000000B00
      4F70656E4469616C6F6731000009006762526563656E74730101000000020000
      00070043617074696F6E0008006C624D61784D52550101000000030000000700
      43617074696F6E0009006C624E756D624D52550000050063624D525501010000
      0004000000070043617074696F6E00050074624D52550000050067624D465501
      0100000005000000070043617074696F6E0008006C624D61784D465501010000
      0006000000070043617074696F6E0009006C624E756D624D4655000005006362
      4D4655010100000007000000070043617074696F6E00050074624D4655000008
      0067624261636B7570010100000008000000070043617074696F6E000B006C62
      4D61784261636B7570010100000009000000070043617074696F6E000C006C62
      4E756D624261636B75700000080063624261636B757001010000000A00000007
      0043617074696F6E00080074624261636B757000001000677270436C65617245
      6C656D656E747301010000000B000000070043617074696F6E000F006C62436C
      656172456C656D656E747301010000000C000000070043617074696F6E000900
      6362526563656E747301010000000D000000070043617074696F6E000A006362
      436C6561724D465501010000000E000000070043617074696F6E000D00636243
      6C6561724261636B757001010000000F000000070043617074696F6E000C0063
      62436C6561724361636865010100000010000000070043617074696F6E000800
      62746E436C656172010100000011000000070043617074696F6E000A00636243
      6C6561724D5255010100000012000000070043617074696F6E00100067624F74
      68657246756E6374696F6E73010100000013000000070043617074696F6E0007
      0063624361636865010100000014000000070043617074696F6E000B00636253
      63686564756C6572010100000015000000070043617074696F6E00}
  end
end
