object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 325
  ClientWidth = 529
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
    Left = 367
    Top = 294
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 446
    Top = 294
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object pnlOptionsPage: TPanel
    Left = 191
    Top = 8
    Width = 331
    Height = 280
    TabOrder = 3
  end
  object vstListCategory: TVirtualStringTree
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
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    OnAddToSelection = vstListCategoryAddToSelection
    OnFreeNode = vstListCategoryFreeNode
    OnGetText = vstListCategoryGetText
    OnGetImageIndex = vstListCategoryGetImageIndex
    OnInitNode = vstListCategoryInitNode
    Columns = <>
  end
end
