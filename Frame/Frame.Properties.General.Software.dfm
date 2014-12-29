inherited frmSWGeneralPropertyPage: TfrmSWGeneralPropertyPage
  inherited gbItem: TGroupBox
    inherited edtName: TEdit
      Width = 161
      ExplicitWidth = 161
    end
  end
  object grpSoftware: TGroupBox
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
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 8
    Top = 248
    LangData = {
      180066726D535747656E6572616C50726F706572747950616765000110000000
      060067624974656D010100000001000000070043617074696F6E0006006C624E
      616D65010100000002000000070043617074696F6E000A006C62506174684963
      6F6E010100000003000000070043617074696F6E0007006564744E616D650000
      0B006564745061746849636F6E00000D0062746E42726F77736549636F6E0101
      00000005000000070043617074696F6E000B00677270536F6674776172650101
      00000006000000070043617074696F6E000C006C62576F726B696E6744697201
      0100000007000000070043617074696F6E0007006C62496E666F320101000000
      08000000070043617074696F6E000C006C62506172616D657465727301010000
      0009000000070043617074696F6E0009006C625061746845786501010000000A
      000000070043617074696F6E00130062746E42726F777365576F726B696E6744
      697201010000000B000000070043617074696F6E000D00656474576F726B696E
      6744697200000C0062746E42726F77736545786501010000000D000000070043
      617074696F6E000D00656474506172616D657465727300000A00656474506174
      684578650000}
  end
end
