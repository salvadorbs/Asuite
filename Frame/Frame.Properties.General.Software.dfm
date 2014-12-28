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
      TabOrder = 3
      OnClick = btnBrowseWorkingDirClick
    end
    object edtWorkingDir: TEdit
      Left = 8
      Top = 64
      Width = 267
      Height = 21
      TabOrder = 2
      Text = '$ASuite\'
    end
    object btnBrowseExe: TButton
      Left = 281
      Top = 30
      Width = 65
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnBrowseExeClick
    end
    object edtParameters: TEdit
      Left = 8
      Top = 98
      Width = 267
      Height = 21
      TabOrder = 4
    end
    object edtPathExe: TEdit
      Left = 8
      Top = 30
      Width = 267
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = '$ASuite\'
      OnExit = edtPathExeExit
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 8
    Top = 248
    LangData = {
      180066726D535747656E6572616C50726F706572747950616765000111000000
      0B004F70656E4469616C6F67310000060067624974656D010100000001000000
      070043617074696F6E0006006C624E616D650101000000020000000700436170
      74696F6E000A006C625061746849636F6E010100000003000000070043617074
      696F6E0007006564744E616D6500000B006564745061746849636F6E00000D00
      62746E42726F77736549636F6E010100000005000000070043617074696F6E00
      0B00677270536F667477617265010100000006000000070043617074696F6E00
      0C006C62576F726B696E67446972010100000007000000070043617074696F6E
      0007006C62496E666F32010100000008000000070043617074696F6E000C006C
      62506172616D6574657273010100000009000000070043617074696F6E000900
      6C625061746845786501010000000A000000070043617074696F6E0013006274
      6E42726F777365576F726B696E6744697201010000000B000000070043617074
      696F6E000D00656474576F726B696E6744697200000C0062746E42726F777365
      45786501010000000D000000070043617074696F6E000D00656474506172616D
      657465727300000A00656474506174684578650000}
  end
end
