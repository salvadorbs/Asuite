object frFontSettings: TfrFontSettings
  Left = 0
  Top = 0
  Width = 312
  Height = 57
  AutoScroll = False
  TabOrder = 0
  object gbMain: TGroupBox
    Left = 0
    Top = 0
    Width = 312
    Height = 57
    Align = alClient
    TabOrder = 0
    DesignSize = (
      312
      57)
    object pSample: TPanel
      Left = 8
      Top = 16
      Width = 213
      Height = 33
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvLowered
      Caption = 'This is a sample text'
      TabOrder = 0
    end
    object bSelectFont: TButton
      Left = 228
      Top = 20
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Select...'
      TabOrder = 1
      OnClick = bSelectFontClick
    end
  end
  object lcMain: TDKLanguageController
    SectionName = 'frFontSettings'
    Left = 180
    Top = 16
    LangData = {
      0E006672466F6E7453657474696E67730001030000000B006253656C65637446
      6F6E74010100000001000000070043617074696F6E0007007053616D706C6501
      0100000002000000070043617074696F6E00060067624D61696E0000}
  end
end
