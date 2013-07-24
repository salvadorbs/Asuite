inherited frmSWGeneralPropertyPage: TfrmSWGeneralPropertyPage
  inherited gbItem: TGroupBox
    inherited edtName: TEdit
      Width = 161
      ExplicitWidth = 161
    end
  end
  object grpSoftware: TGroupBox [1]
    Left = 3
    Top = 88
    Width = 354
    Height = 184
    Caption = 'Path'
    TabOrder = 1
    object lbWorkingDir: TLabel
      Left = 8
      Top = 51
      Width = 171
      Height = 13
      Caption = 'Custom working directory (optional)'
      Color = clBtnFace
      ParentColor = False
    end
    object lbInfo2: TLabel
      Left = 8
      Top = 125
      Width = 338
      Height = 52
      AutoSize = False
      Caption = 
        'Note: You can use these vars (included environment variables) in' +
        ' relative path:'#13#10'- $asuite = ASuite folder path ( %s )'#13#10'- $drive' +
        ' = drive path ( %s )'
      Color = clBtnFace
      ParentColor = False
      WordWrap = True
    end
    object lbParameters: TLabel
      Left = 8
      Top = 85
      Width = 104
      Height = 13
      Caption = 'Parameters (optional)'
      Color = clBtnFace
      ParentColor = False
    end
    object lbPathExe: TLabel
      Left = 8
      Top = 17
      Width = 161
      Height = 13
      Caption = 'Executable/folder/web page path'
      Color = clBtnFace
      ParentColor = False
    end
    object btnBrowseWorkingDir: TButton
      Left = 281
      Top = 64
      Width = 65
      Height = 21
      Caption = 'Browse'
      TabOrder = 0
    end
    object edtWorkingDir: TEdit
      Left = 8
      Top = 64
      Width = 267
      Height = 21
      TabOrder = 1
      Text = '$ASuite\'
    end
    object btnBrowseExe: TButton
      Left = 281
      Top = 30
      Width = 65
      Height = 21
      Caption = 'Browse'
      TabOrder = 2
    end
    object edtParameters: TEdit
      Left = 8
      Top = 98
      Width = 267
      Height = 21
      TabOrder = 3
    end
    object edtPathExe: TEdit
      Left = 8
      Top = 30
      Width = 267
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = '$ASuite\'
    end
  end
end
