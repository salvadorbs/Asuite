object SpellCheckForm: TSpellCheckForm
  Left = 453
  Top = 413
  BorderStyle = bsSingle
  Caption = 'Spell Check'
  ClientHeight = 219
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 19
    Top = 15
    Width = 78
    Height = 13
    Alignment = taRightJustify
    Caption = '&Not in dictionary'
    FocusControl = Edit
  end
  object Label2: TLabel
    Left = 56
    Top = 46
    Width = 39
    Height = 13
    Alignment = taRightJustify
    Caption = 'Suggest'
  end
  object Edit: TEdit
    Left = 104
    Top = 13
    Width = 151
    Height = 21
    TabOrder = 0
  end
  object btnSkip: TButton
    Left = 280
    Top = 8
    Width = 73
    Height = 33
    Caption = '&Skip'
    TabOrder = 1
    OnClick = btnClick
  end
  object btnSkipAll: TButton
    Left = 360
    Top = 8
    Width = 73
    Height = 33
    Caption = 'Skip &All'
    TabOrder = 2
    OnClick = btnClick
  end
  object ListBox: TListBox
    Left = 104
    Top = 40
    Width = 153
    Height = 161
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBoxClick
  end
  object btnAddDictionary: TButton
    Left = 280
    Top = 56
    Width = 105
    Height = 33
    Caption = 'Add to &Dictionary'
    TabOrder = 4
    OnClick = btnClick
  end
  object btnReplace: TButton
    Left = 280
    Top = 104
    Width = 73
    Height = 33
    Caption = '&Replace'
    TabOrder = 5
    OnClick = btnClick
  end
  object btnReplaceAll: TButton
    Left = 360
    Top = 104
    Width = 73
    Height = 33
    Caption = 'Replace A&ll'
    TabOrder = 6
    OnClick = btnClick
  end
  object btnClose: TButton
    Left = 280
    Top = 168
    Width = 73
    Height = 33
    Caption = '&Close'
    Default = True
    TabOrder = 7
    OnClick = btnClick
  end
end
