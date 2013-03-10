object frmRunAs: TfrmRunAs
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Run as'
  ClientHeight = 129
  ClientWidth = 249
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
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 233
    Height = 81
    TabOrder = 0
    object lbUsername: TLabel
      Left = 8
      Top = 16
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object lbPassword: TLabel
      Left = 8
      Top = 50
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object edtUsername: TEdit
      Left = 104
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edtPassword: TEdit
      Left = 104
      Top = 47
      Width = 121
      Height = 21
      TabOrder = 1
    end
  end
  object btnOk: TButton
    Left = 80
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 166
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
