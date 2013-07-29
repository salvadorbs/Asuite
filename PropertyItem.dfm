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
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 476
    Top = 294
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object pnlPropertyPage: TPanel
    Left = 191
    Top = 8
    Width = 361
    Height = 280
    TabOrder = 2
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
    TabOrder = 3
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    OnAddToSelection = vstCategoryAddToSelection
    OnFreeNode = vstCategoryFreeNode
    OnGetText = vstCategoryGetText
    OnGetImageIndex = vstCategoryGetImageIndex
    OnInitNode = vstCategoryInitNode
    Columns = <>
  end
end
