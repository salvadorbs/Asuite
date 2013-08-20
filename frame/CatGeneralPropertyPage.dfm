inherited frmCatGeneralPropertyPage: TfrmCatGeneralPropertyPage
  object grpSubItems: TGroupBox [1]
    Left = 3
    Top = 89
    Width = 354
    Height = 182
    Caption = 'List softwares'
    TabOrder = 1
    object lblNote: TLabel
      Left = 8
      Top = 136
      Width = 335
      Height = 39
      Caption = 
        'Note: In this list you can find all software items of this categ' +
        'ory (only first level). Uncheck items who you don'#39't want to star' +
        't, when run this category.'
      WordWrap = True
    end
    object vstCategoryItems: TVirtualStringTree
      Left = 8
      Top = 16
      Width = 338
      Height = 114
      DragOperations = []
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      Images = ImagesDM.IcoImages
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
      OnGetText = vstCategoryItemsGetText
      OnGetImageIndex = vstCategoryItemsGetImageIndex
      Columns = <>
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 8
    Top = 248
    LangData = {
      190066726D43617447656E6572616C50726F70657274795061676500010A0000
      000B004F70656E4469616C6F67310000060067624974656D0101000000010000
      00070043617074696F6E0006006C624E616D6501010000000200000007004361
      7074696F6E000A006C625061746849636F6E0101000000030000000700436170
      74696F6E0007006564744E616D6500000B006564745061746849636F6E00000D
      0062746E42726F77736549636F6E010100000005000000070043617074696F6E
      000B006772705375624974656D73010100000006000000070043617074696F6E
      0007006C626C4E6F7465010100000007000000070043617074696F6E00100076
      737443617465676F72794974656D730000}
  end
end
