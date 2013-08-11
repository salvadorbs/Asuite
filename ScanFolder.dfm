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
    TabOrder = 0
    OnClick = btnScanClick
  end
  object btnCancel: TButton
    Left = 262
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object pnlScan: TPanel
    Left = 8
    Top = 8
    Width = 329
    Height = 209
    TabOrder = 2
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
      object btnBrowse: TButton
        Left = 239
        Top = 33
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 0
        OnClick = btnBrowseClick
      end
      object edtFolderPath: TEdit
        Left = 8
        Top = 33
        Width = 225
        Height = 21
        TabOrder = 1
      end
      object cbSubfolders: TCheckBox
        Left = 8
        Top = 58
        Width = 177
        Height = 17
        Caption = 'Scan subfolders'
        TabOrder = 2
      end
    end
  end
end
