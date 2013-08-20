object dDKL_ConstEditor: TdDKL_ConstEditor
  Left = 357
  Top = 191
  ActiveControl = gMain
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'DKLang Constant Editor'
  ClientHeight = 435
  ClientWidth = 592
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    592
    435)
  PixelsPerInch = 96
  TextHeight = 13
  object lCount: TLabel
    Left = 8
    Top = 408
    Width = 43
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '<count>'
  end
  object lDeleteHint: TLabel
    Left = 8
    Top = 368
    Width = 213
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Use Ctrl+Delete to delete the current entry.'
  end
  object gMain: TStringGrid
    Left = 8
    Top = 8
    Width = 577
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = gMainKeyDown
    OnMouseUp = gMainMouseUp
    OnSelectCell = gMainSelectCell
    ColWidths = (
      286
      284)
  end
  object bOK: TButton
    Left = 188
    Top = 404
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 268
    Top = 404
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object bLoad: TButton
    Left = 428
    Top = 404
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Load...'
    TabOrder = 5
    OnClick = bLoadClick
  end
  object bSave: TButton
    Left = 508
    Top = 404
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Save...'
    TabOrder = 6
    OnClick = bSaveClick
  end
  object cbSaveToLangSource: TCheckBox
    Left = 8
    Top = 384
    Width = 573
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 
      '&Also save the constants into the project language source file (' +
      '*.dklang)'
    TabOrder = 1
  end
  object bErase: TButton
    Left = 348
    Top = 404
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Erase'
    TabOrder = 4
    OnClick = bEraseClick
  end
end
