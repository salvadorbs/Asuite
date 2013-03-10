object KPrintSetupForm: TKPrintSetupForm
  Left = 808
  Top = 247
  ActiveControl = CBFitToPage
  BorderStyle = bsDialog
  Caption = 'Page setup'
  ClientHeight = 357
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GBFileToPrint: TGroupBox
    Left = 8
    Top = 8
    Width = 449
    Height = 45
    Caption = 'Title of printed document:'
    TabOrder = 0
    object EDTitle: TEdit
      Left = 8
      Top = 16
      Width = 432
      Height = 21
      TabOrder = 0
      Text = 'EDTitle'
    end
  end
  object GBPrintOptions: TGroupBox
    Left = 8
    Top = 109
    Width = 249
    Height = 105
    Caption = 'Print options:'
    TabOrder = 1
    object Label1: TLabel
      Left = 162
      Top = 23
      Width = 29
      Height = 13
      Caption = 'Scale:'
      Color = clBtnFace
      FocusControl = EDPrintScale
      ParentColor = False
    end
    object CBFitToPage: TCheckBox
      Left = 8
      Top = 21
      Width = 70
      Height = 17
      Caption = '&Fit to page'
      TabOrder = 0
      OnClick = EDTopExit
    end
    object CBPageNumbers: TCheckBox
      Left = 8
      Top = 40
      Width = 86
      Height = 17
      Caption = 'Pa&ge numbers'
      TabOrder = 1
      OnClick = CBPageNumbersClick
    end
    object CBUseColor: TCheckBox
      Left = 8
      Top = 59
      Width = 62
      Height = 17
      Caption = '&Use color'
      TabOrder = 2
      OnClick = CBPageNumbersClick
    end
    object EDPrintScale: TEdit
      Left = 162
      Top = 39
      Width = 48
      Height = 21
      TabOrder = 3
      OnExit = EDTopExit
    end
    object CBPaintSelection: TCheckBox
      Left = 8
      Top = 78
      Width = 87
      Height = 17
      Caption = 'Pa&int selection'
      TabOrder = 4
      OnClick = CBPageNumbersClick
    end
    object CBPrintTitle: TCheckBox
      Left = 134
      Top = 78
      Width = 61
      Height = 17
      Caption = 'Print tit&le'
      TabOrder = 5
      OnClick = CBPageNumbersClick
    end
  end
  object BUPrint: TButton
    Left = 89
    Top = 325
    Width = 74
    Height = 25
    Caption = '&Print'
    TabOrder = 4
    OnClick = BUPrintClick
  end
  object BUCancel: TButton
    Left = 383
    Top = 325
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object GBMargins: TGroupBox
    Left = 264
    Top = 109
    Width = 193
    Height = 211
    Caption = 'Margins:'
    TabOrder = 3
    object LBMarginUnits: TLabel
      Left = 8
      Top = 23
      Width = 62
      Height = 13
      Caption = 'Margin u&nits:'
      Color = clBtnFace
      FocusControl = CoBMarginUnits
      ParentColor = False
    end
    object LBLeft: TLabel
      Left = 8
      Top = 86
      Width = 23
      Height = 13
      Caption = 'Left:'
      Color = clBtnFace
      FocusControl = EDLeft
      ParentColor = False
    end
    object LBRight: TLabel
      Left = 102
      Top = 86
      Width = 29
      Height = 13
      Caption = 'Right:'
      Color = clBtnFace
      FocusControl = EDRight
      ParentColor = False
    end
    object LBTop: TLabel
      Left = 9
      Top = 131
      Width = 22
      Height = 13
      Caption = 'Top:'
      Color = clBtnFace
      FocusControl = EDTop
      ParentColor = False
    end
    object LBBottom: TLabel
      Left = 102
      Top = 131
      Width = 38
      Height = 13
      Caption = 'Bottom:'
      Color = clBtnFace
      FocusControl = EDBottom
      ParentColor = False
    end
    object LBUnitsLeft: TLabel
      Left = 58
      Top = 105
      Width = 7
      Height = 13
      Caption = 'A'
      Color = clBtnFace
      ParentColor = False
    end
    object LBUnitsTop: TLabel
      Left = 58
      Top = 150
      Width = 7
      Height = 13
      Caption = 'A'
      Color = clBtnFace
      ParentColor = False
    end
    object LBUnitsRight: TLabel
      Left = 152
      Top = 105
      Width = 7
      Height = 13
      Caption = 'A'
      Color = clBtnFace
      ParentColor = False
    end
    object LBUnitsBottom: TLabel
      Left = 152
      Top = 150
      Width = 7
      Height = 13
      Caption = 'A'
      Color = clBtnFace
      ParentColor = False
    end
    object CoBMarginUnits: TComboBox
      Left = 8
      Top = 39
      Width = 176
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = CoBMarginUnitsChange
      Items.Strings = (
        'milimeters'
        'centimeters'
        'inches'
        'hundredths of inches')
    end
    object CBMirrorMargins: TCheckBox
      Left = 8
      Top = 181
      Width = 86
      Height = 17
      Caption = '&Mirror margins'
      TabOrder = 5
      OnClick = CBPageNumbersClick
    end
    object EDLeft: TEdit
      Left = 8
      Top = 102
      Width = 48
      Height = 21
      TabOrder = 1
      OnExit = EDTopExit
    end
    object EDRight: TEdit
      Left = 102
      Top = 102
      Width = 48
      Height = 21
      TabOrder = 2
      OnExit = EDTopExit
    end
    object EDTop: TEdit
      Left = 8
      Top = 147
      Width = 48
      Height = 21
      TabOrder = 3
      OnExit = EDTopExit
    end
    object EDBottom: TEdit
      Left = 102
      Top = 147
      Width = 48
      Height = 21
      TabOrder = 4
      OnExit = EDTopExit
    end
  end
  object GBPageSelection: TGroupBox
    Left = 8
    Top = 215
    Width = 249
    Height = 105
    Caption = 'Page selection:'
    TabOrder = 2
    object LBRangeTo: TLabel
      Left = 163
      Top = 51
      Width = 14
      Height = 13
      Caption = 'to:'
      Color = clBtnFace
      ParentColor = False
    end
    object LBCopies: TLabel
      Left = 8
      Top = 78
      Width = 87
      Height = 13
      Caption = 'Number of &copies:'
      Color = clBtnFace
      FocusControl = EDCopies
      ParentColor = False
    end
    object RBAll: TRadioButton
      Left = 8
      Top = 22
      Width = 61
      Height = 17
      Caption = '&All pages'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBAllClick
    end
    object RBRange: TRadioButton
      Left = 8
      Top = 48
      Width = 78
      Height = 17
      Caption = '&Range from:'
      TabOrder = 1
      OnClick = RBAllClick
    end
    object RBSelectedOnly: TRadioButton
      Left = 128
      Top = 22
      Width = 82
      Height = 17
      Caption = 'Selected &only'
      TabOrder = 2
      OnClick = RBAllClick
    end
    object EDRangeFrom: TEdit
      Left = 108
      Top = 46
      Width = 48
      Height = 21
      TabOrder = 3
      OnExit = EDTopExit
    end
    object EDRangeTo: TEdit
      Left = 193
      Top = 46
      Width = 48
      Height = 21
      TabOrder = 4
      OnExit = EDTopExit
    end
    object EDCopies: TEdit
      Left = 126
      Top = 73
      Width = 48
      Height = 21
      TabOrder = 5
    end
    object CBCollate: TCheckBox
      Left = 179
      Top = 75
      Width = 51
      Height = 17
      Caption = 'Collate'
      TabOrder = 6
      OnClick = CBPageNumbersClick
    end
  end
  object BUPreview: TButton
    Left = 8
    Top = 325
    Width = 75
    Height = 25
    Caption = 'Previe&w...'
    TabOrder = 6
    OnClick = BUPreviewClick
  end
  object BUOk: TButton
    Left = 303
    Top = 325
    Width = 74
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object GBPrinter: TGroupBox
    Left = 8
    Top = 56
    Width = 449
    Height = 50
    Caption = 'Printer settings'
    TabOrder = 8
    object LBPrinterName: TLabel
      Left = 8
      Top = 20
      Width = 65
      Height = 13
      Caption = 'Printer name:'
      Color = clBtnFace
      FocusControl = EDCopies
      ParentColor = False
    end
    object CoBPrinterName: TComboBox
      Left = 112
      Top = 17
      Width = 206
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'CoBPrinterName'
      OnChange = EDTopExit
    end
    object BUConfigure: TButton
      Left = 328
      Top = 15
      Width = 113
      Height = 25
      Caption = 'Configure...'
      TabOrder = 1
      OnClick = BUConfigureClick
    end
  end
  object PSDMain: TPrinterSetupDialog
    Left = 416
    Top = 29
  end
end
