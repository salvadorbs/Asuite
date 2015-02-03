object frmDialogBase: TfrmDialogBase
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'frmDialogBase'
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
  DesignSize = (
    559
    325)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 398
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 477
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object pnlDialogPage: TPanel
    Left = 191
    Top = 8
    Width = 361
    Height = 280
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object vstCategory: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 177
    Height = 280
    Anchors = [akLeft, akTop, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 1
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    Columns = <>
  end
end
