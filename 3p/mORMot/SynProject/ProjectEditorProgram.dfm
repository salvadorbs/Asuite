object ProjectEditorProgramForm: TProjectEditorProgramForm
  Left = 47
  Top = 204
  Width = 903
  Height = 450
  Caption = ' SynProject - Integrated Reference Class Browser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 895
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      895
      41)
    object lUnitName: TLabel
      Left = 230
      Top = 13
      Width = 53
      Height = 13
      Alignment = taRightJustify
      Caption = 'Unit Name:'
    end
    object lSADSection: TLabel
      Left = 29
      Top = 13
      Width = 62
      Height = 13
      Alignment = taRightJustify
      Caption = 'SAD Section:'
    end
    object cbUnitName: TComboBox
      Left = 287
      Top = 10
      Width = 170
      Height = 21
      Style = csDropDownList
      DropDownCount = 32
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbUnitNameChange
    end
    object btnOk: TButton
      Left = 713
      Top = 8
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save'
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 809
      Top = 8
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object cbSADSection: TComboBox
      Left = 96
      Top = 10
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbSADSectionChange
    end
    object leUnitDescription: TLabeledEdit
      Left = 552
      Top = 10
      Width = 148
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 79
      EditLabel.Height = 13
      EditLabel.Caption = 'Unit Description:'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 4
      OnExit = leUnitDescriptionExit
    end
  end
  object pagTypes: TPageControl
    Left = 0
    Top = 41
    Width = 710
    Height = 375
    Align = alClient
    TabOrder = 1
  end
  object PanelRight: TPanel
    Left = 710
    Top = 41
    Width = 185
    Height = 375
    Align = alRight
    TabOrder = 2
    DesignSize = (
      185
      375)
    object l1: TLabel
      Left = 8
      Top = 328
      Width = 166
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Double-click any left item to add it.'
    end
    object lstValues: TListBox
      Left = 1
      Top = 1
      Width = 183
      Height = 320
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstValuesClick
    end
    object btnDeleteItem: TButton
      Left = 8
      Top = 344
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete item'
      TabOrder = 1
      OnClick = btnDeleteItemClick
    end
    object btnClearList: TButton
      Left = 88
      Top = 344
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Clear list'
      TabOrder = 2
      OnClick = btnClearListClick
    end
  end
end
