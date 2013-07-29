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
end
