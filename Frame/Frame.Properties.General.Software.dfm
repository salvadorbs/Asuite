inherited frmSWGeneralPropertyPage: TfrmSWGeneralPropertyPage
  inherited gbItem: TGroupBox
    Font.Style = [fsBold]
    ParentFont = False
    inherited lbName: TLabel
      Width = 32
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 32
    end
    inherited lbPathIcon: TLabel
      Width = 157
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 157
    end
    inherited edtName: TEdit
      Width = 161
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 161
    end
    inherited edtPathIcon: TJvFilenameEdit
      OnBeforeDialog = nil
      OnAfterDialog = nil
      TabOrder = 2
      OnExit = nil
    end
    object btnExtractName: TButton
      Left = 216
      Top = 24
      Width = 130
      Height = 21
      Caption = 'Extract name from exe'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnExtractNameClick
    end
  end
  object grpSoftware: TGroupBox
    Left = 3
    Top = 88
    Width = 354
    Height = 184
    Caption = 'Path'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbWorkingDir: TLabel
      Left = 8
      Top = 51
      Width = 171
      Height = 13
      Caption = 'Custom working directory (optional)'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbInfo2: TLabel
      Left = 8
      Top = 124
      Width = 338
      Height = 52
      AutoSize = False
      Caption = 
        'Note: You can use these vars (included environment variables) in' +
        ' relative path:'#13#10'- $asuite = ASuite folder path ( %s )'#13#10'- $drive' +
        ' = drive path ( %s )'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object lbParameters: TLabel
      Left = 8
      Top = 85
      Width = 104
      Height = 13
      Caption = 'Parameters (optional)'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbPathExe: TLabel
      Left = 8
      Top = 17
      Width = 161
      Height = 13
      Caption = 'Executable/folder/web page path'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object edtParameters: TEdit
      Left = 8
      Top = 98
      Width = 267
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object edtWorkingDir: TJvDirectoryEdit
      Left = 8
      Top = 64
      Width = 338
      Height = 21
      OnBeforeDialog = edtWorkingDirBeforeDialog
      OnAfterDialog = edtWorkingDirAfterDialog
      DialogKind = dkWin32
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '$ASuite\'
      OnChange = edtWorkingDirChange
    end
    object edtPathExe: TJvFilenameEdit
      Left = 8
      Top = 30
      Width = 338
      Height = 21
      OnBeforeDialog = edtPathExeBeforeDialog
      OnAfterDialog = edtPathExeAfterDialog
      AddQuotes = False
      DialogOptions = [ofHideReadOnly, ofEnableSizing]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '$ASuite\'
      OnChange = edtPathExeChange
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 8
    Top = 248
    LangData = {
      180066726D535747656E6572616C50726F70657274795061676500010E000000
      060067624974656D010100000001000000070043617074696F6E0006006C624E
      616D65010100000002000000070043617074696F6E000A006C62506174684963
      6F6E010100000003000000070043617074696F6E0007006564744E616D650000
      0B00677270536F667477617265010100000006000000070043617074696F6E00
      0C006C62576F726B696E67446972010100000007000000070043617074696F6E
      0007006C62496E666F32010100000008000000070043617074696F6E000C006C
      62506172616D6574657273010100000009000000070043617074696F6E000900
      6C625061746845786501010000000A000000070043617074696F6E000D006564
      74506172616D657465727300000B006564745061746849636F6E00000D006564
      74576F726B696E6744697200000A006564745061746845786500000E0062746E
      457874726163744E616D6501010000000B000000070043617074696F6E00}
  end
end
