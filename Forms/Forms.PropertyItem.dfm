object frmPropertyItem: TfrmPropertyItem
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Property'
  ClientHeight = 325
  ClientWidth = 559
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
  object btnOk: TButton
    Left = 397
    Top = 294
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 476
    Top = 294
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object pnlPropertyPage: TPanel
    Left = 191
    Top = 8
    Width = 361
    Height = 280
    TabOrder = 0
  end
  object vstCategory: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 177
    Height = 280
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = ImagesDM.LargeIcoImages
    TabOrder = 1
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    OnAddToSelection = vstCategoryAddToSelection
    OnFreeNode = vstCategoryFreeNode
    OnGetText = vstCategoryGetText
    OnGetImageIndex = vstCategoryGetImageIndex
    OnInitNode = vstCategoryInitNode
    Columns = <>
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 288
    LangData = {
      0F0066726D50726F70657274794974656D010100000001000000070043617074
      696F6E0104000000050062746E4F6B010100000002000000070043617074696F
      6E00090062746E43616E63656C010100000003000000070043617074696F6E00
      0F00706E6C50726F70657274795061676500000B0076737443617465676F7279
      0000}
  end
end
