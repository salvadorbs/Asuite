object fMDIChild: TfMDIChild
  Left = 467
  Top = 341
  Width = 288
  Height = 148
  Caption = '<caption>'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lSampleMessage: TLabel
    Left = 12
    Top = 12
    Width = 123
    Height = 13
    Caption = 'This is a sample message.'
  end
  object bCancel: TButton
    Left = 104
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 0
  end
  object lcMain: TDKLanguageController
    IgnoreList.Strings = (
      '.Caption')
    SectionName = 'fMDIChild'
    Left = 240
    Top = 48
    LangData = {
      0900664D44494368696C6400010200000007006243616E63656C010100000003
      000000070043617074696F6E000E006C53616D706C654D657373616765010100
      000004000000070043617074696F6E00}
  end
end

