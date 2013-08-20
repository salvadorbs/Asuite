object fMain: TfMain
  Left = 438
  Top = 199
  BorderStyle = bsDialog
  Caption = 'Sample Form'
  ClientHeight = 111
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial Unicode MS'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object lSampleMessage: TLabel
    Left = 19
    Top = 41
    Width = 127
    Height = 14
    Caption = 'This is a sample message.'
  end
  object bCancel: TButton
    Left = 97
    Top = 71
    Width = 70
    Height = 23
    Caption = 'Cancel'
    TabOrder = 0
  end
  object cbLanguage: TComboBox
    Left = 7
    Top = 7
    Width = 244
    Height = 22
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbLanguageChange
  end
  object lcMain: TDKLanguageController
    Left = 232
    Top = 48
    LangData = {
      0500664D61696E010100000001000000070043617074696F6E01030000000700
      6243616E63656C010100000003000000070043617074696F6E000E006C53616D
      706C654D657373616765010100000004000000070043617074696F6E000A0063
      624C616E67756167650000}
  end
end
