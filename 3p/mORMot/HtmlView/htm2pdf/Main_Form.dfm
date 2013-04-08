object MainForm: TMainForm
  Left = 152
  Top = 180
  Width = 903
  Height = 632
  Caption = 'Html2Pdf'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    887
    594)
  PixelsPerInch = 96
  TextHeight = 13
  object lbHTML: TLabel
    Left = 89
    Top = 12
    Width = 709
    Height = 15
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lbPDF: TLabel
    Left = 170
    Top = 565
    Width = 493
    Height = 15
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lbMargins: TLabel
    Left = 494
    Top = 539
    Width = 170
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'Margins Left, Top, Right, Bottom:'
  end
  object btnOpen: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object Viewer: THTMLViewer
    Left = 8
    Top = 39
    Width = 871
    Height = 491
    Cursor = crDefault
    TabOrder = 1
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefBackground = clWindow
    BorderStyle = htSingle
    HistoryMaxCount = 0
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    NoSelect = True
    CharSet = DEFAULT_CHARSET
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintMarginBottom = 2.000000000000000000
    PrintScale = 1.000000000000000000
  end
  object btnSave: TButton
    Left = 89
    Top = 561
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object btnSaveAs: TButton
    Left = 8
    Top = 561
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save As'
    TabOrder = 3
    OnClick = btnSaveAsClick
  end
  object cxOpenAfterSave: TCheckBox
    Left = 8
    Top = 538
    Width = 156
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Open after save'
    TabOrder = 4
  end
  object edMarginLeft: TEdit
    Left = 669
    Top = 536
    Width = 48
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 5
  end
  object edMarginTop: TEdit
    Left = 723
    Top = 536
    Width = 48
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 6
  end
  object edMarginRight: TEdit
    Left = 777
    Top = 536
    Width = 48
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 7
  end
  object edMarginBottom: TEdit
    Left = 831
    Top = 536
    Width = 48
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 8
  end
  object cxScaleToFit: TCheckBox
    Left = 384
    Top = 538
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Scale to fit'
    TabOrder = 9
  end
  object cbOrientation: TComboBox
    Left = 264
    Top = 536
    Width = 85
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 10
    Text = 'Portrait'
    Items.Strings = (
      'Portrait'
      'Landscape')
  end
  object cbPaperSize: TComboBox
    Left = 195
    Top = 536
    Width = 63
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 11
    Text = 'A4'
    Items.Strings = (
      'A4'
      'A5'
      'A3'
      'Letter'
      'Legal')
  end
  object btnSynopse: TButton
    Left = 760
    Top = 561
    Width = 121
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'http://synopse.info'
    TabOrder = 12
    OnClick = btnSynopseClick
  end
  object cxPrintPage: TCheckBox
    Left = 673
    Top = 565
    Width = 83
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'with Page #'
    TabOrder = 13
  end
  object OpenDialog: TOpenDialog
    Filter = 'HTML Files|*.htm?'
    Left = 24
    Top = 48
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.pdf'
    Filter = 'PDF Files|*.pdf'
    Left = 90
    Top = 48
  end
end
