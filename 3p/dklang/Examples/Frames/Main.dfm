object fMain: TfMain
  Left = 438
  Top = 199
  ActiveControl = cbLanguage
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 284
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    338
    284)
  PixelsPerInch = 96
  TextHeight = 13
  object lLanguage: TLabel
    Left = 12
    Top = 4
    Width = 96
    Height = 13
    Caption = '&Interface language:'
    FocusControl = cbLanguage
  end
  object bCancel: TButton
    Left = 254
    Top = 251
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 0
  end
  object bOK: TButton
    Left = 174
    Top = 251
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
  end
  inline frFontSettings_Table: TfrFontSettings
    Left = 12
    Top = 48
    Width = 312
    Height = 57
    TabOrder = 2
    ExplicitLeft = 12
    ExplicitTop = 48
    inherited lcMain: TDKLanguageController
      LangData = {
        14006672466F6E7453657474696E67735F5461626C650001030000000B006253
        656C656374466F6E74010100000001000000070043617074696F6E0007007053
        616D706C65010100000002000000070043617074696F6E00060067624D61696E
        0000}
    end
  end
  inline frFontSettings_Toolbar: TfrFontSettings
    Left = 12
    Top = 116
    Width = 312
    Height = 57
    TabOrder = 3
    ExplicitLeft = 12
    ExplicitTop = 116
    inherited lcMain: TDKLanguageController
      LangData = {
        16006672466F6E7453657474696E67735F546F6F6C6261720001030000000B00
        6253656C656374466F6E74010100000001000000070043617074696F6E000700
        7053616D706C65010100000002000000070043617074696F6E00060067624D61
        696E0000}
    end
  end
  inline frFontSettings_Interface: TfrFontSettings
    Left = 12
    Top = 184
    Width = 312
    Height = 57
    TabOrder = 4
    ExplicitLeft = 12
    ExplicitTop = 184
    inherited lcMain: TDKLanguageController
      LangData = {
        18006672466F6E7453657474696E67735F496E74657266616365000103000000
        0B006253656C656374466F6E74010100000001000000070043617074696F6E00
        07007053616D706C65010100000002000000070043617074696F6E0006006762
        4D61696E0000}
    end
  end
  object cbLanguage: TComboBox
    Left = 12
    Top = 20
    Width = 313
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = cbLanguageChange
  end
  object lcMain: TDKLanguageController
    IgnoreList.Strings = (
      'frFontSettings_*')
    OnLanguageChanged = lcMainLanguageChanged
    Left = 12
    Top = 248
    LangData = {
      0500664D61696E010100000001000000070043617074696F6E01070000000700
      6243616E63656C010100000003000000070043617074696F6E000300624F4B01
      0100000004000000070043617074696F6E0014006672466F6E7453657474696E
      67735F5461626C65000103000000060067624D61696E000007007053616D706C
      6500000B006253656C656374466F6E74000016006672466F6E7453657474696E
      67735F546F6F6C626172000103000000060067624D61696E000007007053616D
      706C6500000B006253656C656374466F6E74000018006672466F6E7453657474
      696E67735F496E74657266616365000103000000060067624D61696E00000700
      7053616D706C6500000B006253656C656374466F6E74000009006C4C616E6775
      616765010100000005000000070043617074696F6E000A0063624C616E677561
      67650000}
  end
end
