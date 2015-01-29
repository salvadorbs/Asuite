object frmScanFolder: TfrmScanFolder
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Scan folder'
  ClientHeight = 254
  ClientWidth = 347
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
  object btnScan: TButton
    Left = 181
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Scan'
    Default = True
    TabOrder = 1
    OnClick = btnScanClick
  end
  object btnCancel: TButton
    Left = 262
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object pnlScan: TPanel
    Left = 8
    Top = 8
    Width = 329
    Height = 209
    TabOrder = 0
    object grpFileTypes: TGroupBox
      Left = 8
      Top = 88
      Width = 154
      Height = 114
      Caption = 'File Types'
      TabOrder = 1
      object btnTypesReplace: TButton
        Left = 88
        Top = 65
        Width = 58
        Height = 17
        Caption = 'Replace'
        TabOrder = 3
        OnClick = btnTypesReplaceClick
      end
      object btnTypesDelete: TButton
        Left = 88
        Top = 86
        Width = 58
        Height = 17
        Caption = 'Delete'
        TabOrder = 4
        OnClick = btnTypesDeleteClick
      end
      object lxTypes: TListBox
        Left = 9
        Top = 19
        Width = 73
        Height = 86
        ItemHeight = 13
        TabOrder = 0
      end
      object btnTypesAdd: TButton
        Left = 88
        Top = 44
        Width = 58
        Height = 17
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnTypesAddClick
      end
      object edtTypes: TEdit
        Left = 88
        Top = 19
        Width = 58
        Height = 21
        TabOrder = 1
      end
    end
    object grpExclude: TGroupBox
      Left = 168
      Top = 88
      Width = 154
      Height = 114
      Caption = 'Exclude file or folder'
      TabOrder = 2
      object lxExclude: TListBox
        Left = 9
        Top = 19
        Width = 73
        Height = 86
        ItemHeight = 13
        TabOrder = 0
      end
      object edtExclude: TEdit
        Left = 88
        Top = 19
        Width = 58
        Height = 21
        TabOrder = 1
      end
      object btnExcludeAdd: TButton
        Left = 88
        Top = 44
        Width = 58
        Height = 17
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnExcludeAddClick
      end
      object btnExcludeReplace: TButton
        Left = 88
        Top = 65
        Width = 58
        Height = 17
        Caption = 'Replace'
        TabOrder = 3
        OnClick = btnExcludeReplaceClick
      end
      object btnExcludeDelete: TButton
        Left = 88
        Top = 86
        Width = 58
        Height = 17
        Caption = 'Delete'
        TabOrder = 4
        OnClick = btnExcludeDeleteClick
      end
    end
    object grpPath: TGroupBox
      Left = 8
      Top = 0
      Width = 314
      Height = 84
      Caption = 'General settings'
      TabOrder = 0
      object lbFolderPath: TLabel
        Left = 9
        Top = 17
        Width = 55
        Height = 13
        Caption = 'Folder path'
      end
      object cbSubfolders: TCheckBox
        Left = 8
        Top = 58
        Width = 177
        Height = 17
        Caption = 'Scan subfolders'
        TabOrder = 1
      end
      object edtFolderPath: TJvDirectoryEdit
        Left = 8
        Top = 33
        Width = 297
        Height = 21
        DialogKind = dkWin32
        DialogOptionsWin32 = [odStatusAvailable, odNewDialogStyle, odShareable]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = ''
      end
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 226
    LangData = {
      0D0066726D5363616E466F6C646572010100000001000000070043617074696F
      6E0113000000090062746E43616E63656C010100000003000000070043617074
      696F6E00070062746E5363616E010100000004000000070043617074696F6E00
      0700706E6C5363616E00000C0067727046696C65547970657301010000000500
      0000070043617074696F6E000F0062746E54797065735265706C616365010100
      000006000000070043617074696F6E000E0062746E547970657344656C657465
      010100000007000000070043617074696F6E0007006C78547970657300000B00
      62746E5479706573416464010100000008000000070043617074696F6E000800
      656474547970657300000A006772704578636C75646501010000000900000007
      0043617074696F6E0009006C784578636C75646500000A006564744578636C75
      646500000D0062746E4578636C75646541646401010000000A00000007004361
      7074696F6E00110062746E4578636C7564655265706C61636501010000000B00
      0000070043617074696F6E00100062746E4578636C75646544656C6574650101
      0000000C000000070043617074696F6E0007006772705061746801010000000D
      000000070043617074696F6E000C006C62466F6C646572506174680101000000
      0E000000070043617074696F6E000C006362537562666F6C6465727301010000
      0010000000070043617074696F6E000D00656474466F6C646572506174680000}
  end
end
