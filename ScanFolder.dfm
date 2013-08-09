object frmScanFolder: TfrmScanFolder
  Left = 0
  Top = 0
  Caption = 'Scan folder'
  ClientHeight = 266
  ClientWidth = 329
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
    Left = 160
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Scan'
    Default = True
    TabOrder = 0
    OnClick = btnScanClick
  end
  object btnCancel: TButton
    Left = 248
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object pnlScan: TPanel
    Left = 8
    Top = 8
    Width = 313
    Height = 217
    TabOrder = 2
    object lbFolderPath: TLabel
      Left = 8
      Top = 8
      Width = 55
      Height = 13
      Caption = 'Folder path'
    end
    object lbTypes: TLabel
      Left = 8
      Top = 72
      Width = 46
      Height = 13
      Caption = 'File types'
    end
    object lbExclude: TLabel
      Left = 160
      Top = 72
      Width = 54
      Height = 13
      Caption = 'Exclude file'
    end
    object edtFolderPath: TEdit
      Left = 8
      Top = 24
      Width = 225
      Height = 21
      TabOrder = 0
    end
    object btnBrowse: TButton
      Left = 240
      Top = 24
      Width = 65
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object lxTypes: TListBox
      Left = 8
      Top = 88
      Width = 73
      Height = 97
      ItemHeight = 13
      TabOrder = 3
    end
    object edtTypes: TEdit
      Left = 88
      Top = 88
      Width = 65
      Height = 21
      TabOrder = 4
    end
    object btnTypesAdd: TButton
      Left = 88
      Top = 120
      Width = 65
      Height = 17
      Caption = 'Add'
      TabOrder = 5
      OnClick = btnTypesAddClick
    end
    object btnTypesReplace: TButton
      Left = 88
      Top = 144
      Width = 65
      Height = 17
      Caption = 'Replace'
      TabOrder = 6
      OnClick = btnTypesReplaceClick
    end
    object btnTypesDelete: TButton
      Left = 88
      Top = 168
      Width = 65
      Height = 17
      Caption = 'Delete'
      TabOrder = 7
      OnClick = btnTypesDeleteClick
    end
    object cbSubfolders: TCheckBox
      Left = 8
      Top = 48
      Width = 297
      Height = 17
      Caption = 'Scan subfolders'
      TabOrder = 2
    end
    object lxExclude: TListBox
      Left = 160
      Top = 88
      Width = 73
      Height = 97
      ItemHeight = 13
      TabOrder = 8
    end
    object edtExclude: TEdit
      Left = 240
      Top = 88
      Width = 65
      Height = 21
      TabOrder = 9
    end
    object btnExcludeAdd: TButton
      Left = 240
      Top = 120
      Width = 65
      Height = 17
      Caption = 'Add'
      TabOrder = 10
      OnClick = btnExcludeAddClick
    end
    object btnExcludeReplace: TButton
      Left = 240
      Top = 144
      Width = 65
      Height = 17
      Caption = 'Replace'
      TabOrder = 11
      OnClick = btnExcludeReplaceClick
    end
    object btnExcludeDelete: TButton
      Left = 240
      Top = 168
      Width = 65
      Height = 17
      Caption = 'Delete'
      TabOrder = 12
      OnClick = btnExcludeDeleteClick
    end
  end
end
