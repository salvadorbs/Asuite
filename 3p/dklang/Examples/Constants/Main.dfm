object fMain: TfMain
  Left = 438
  Top = 199
  BorderStyle = bsDialog
  Caption = 'Sample Form'
  ClientHeight = 119
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lSampleMessage: TLabel
    Left = 20
    Top = 44
    Width = 123
    Height = 13
    Caption = 'This is a sample message.'
  end
  object bTest: TButton
    Left = 104
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = bTestClick
  end
  object cbLanguage: TComboBox
    Left = 8
    Top = 8
    Width = 261
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cbLanguageChange
  end
  object lcMain: TDKLanguageController
    Left = 240
    Top = 48
    LangData = {
      0500664D61696E010100000001000000070043617074696F6E01030000000500
      6254657374010100000003000000070043617074696F6E000E006C53616D706C
      654D657373616765010100000004000000070043617074696F6E000A0063624C
      616E67756167650000}
  end
end
