inherited frmCatGeneralPropertyPage: TfrmCatGeneralPropertyPage
  inherited gbItem: TGroupBox
    inherited edtPathIcon: TJvFilenameEdit
      OnBeforeDialog = nil
      OnAfterDialog = nil
      OnExit = nil
    end
  end
  object grpSubItems: TGroupBox
    Left = 3
    Top = 95
    Width = 354
    Height = 182
    Caption = 'Software in this category'
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
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
      Columns = <>
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Text')
    Left = 8
    Top = 248
    LangData = {
      190066726D43617447656E6572616C50726F7065727479506167650001080000
      00060067624974656D010100000001000000070043617074696F6E0006006C62
      4E616D65010100000002000000070043617074696F6E000A006C625061746849
      636F6E010100000003000000070043617074696F6E0007006564744E616D6500
      000B006772705375624974656D73010100000006000000070043617074696F6E
      0007006C626C4E6F7465010100000007000000070043617074696F6E00100076
      737443617465676F72794974656D7300000B006564745061746849636F6E0000}
  end
end
