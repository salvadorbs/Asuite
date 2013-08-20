object frmPropertySeparator: TfrmPropertySeparator
  Left = 0
  Top = 0
  Caption = 'Properties'
  ClientHeight = 103
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 200
    Top = 71
    Width = 73
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 119
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 265
    Height = 57
    TabOrder = 0
    object lbName: TLabel
      Left = 8
      Top = 8
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Name'
    end
    object edtName: TEdit
      Left = 8
      Top = 24
      Width = 249
      Height = 21
      TabOrder = 0
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 75
    LangData = {
      140066726D50726F7065727479536570617261746F7201010000000100000007
      0043617074696F6E0105000000050062746E4F6B010100000002000000070043
      617074696F6E00090062746E43616E63656C0101000000030000000700436170
      74696F6E00060050616E656C31000006006C624E616D65010100000004000000
      070043617074696F6E0007006564744E616D650000}
  end
end
