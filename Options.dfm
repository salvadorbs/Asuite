object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  Caption = 'Options'
  ClientHeight = 326
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 351
    Top = 294
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 430
    Top = 294
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object pnlOptionsPage: TPanel
    Left = 175
    Top = 8
    Width = 331
    Height = 280
    TabOrder = 3
  end
  object vstListCategory: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 161
    Height = 280
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = ImagesDM.IcoImages
    TabOrder = 0
    OnFreeNode = vstListCategoryFreeNode
    OnGetText = vstListCategoryGetText
    OnInitNode = vstListCategoryInitNode
    OnNodeClick = vstListCategoryNodeClick
    Columns = <>
  end
  object OpenDialog1: TOpenDialog
    Left = 8
    Top = 296
  end
end
