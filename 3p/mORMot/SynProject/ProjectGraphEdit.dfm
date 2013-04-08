object GraphEditForm: TGraphEditForm
  Left = 278
  Top = 260
  BorderStyle = bsSingle
  ClientHeight = 490
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 323
    Top = 0
    Width = 458
    Height = 490
    Align = alClient
    Proportional = True
  end
  object Splitter1: TSplitter
    Left = 321
    Top = 0
    Width = 2
    Height = 490
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 490
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      321
      490)
    object Label1: TLabel
      Left = 24
      Top = 42
      Width = 20
      Height = 13
      Alignment = taRightJustify
      Caption = 'Title'
    end
    object Label2: TLabel
      Left = 19
      Top = 12
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = 'Name'
    end
    object Label3: TLabel
      Left = 8
      Top = 392
      Width = 297
      Height = 54
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Tree: TTreeView
      Left = 8
      Top = 72
      Width = 305
      Height = 313
      Anchors = [akLeft, akTop, akRight, akBottom]
      Indent = 19
      MultiSelect = True
      MultiSelectStyle = [msControlSelect, msShiftSelect]
      ReadOnly = True
      TabOrder = 7
      Visible = False
      OnClick = TreeClick
    end
    object EditName: TEdit
      Left = 48
      Top = 8
      Width = 201
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object EditTitle: TEdit
      Left = 48
      Top = 40
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object Source: TMemo
      Left = 8
      Top = 72
      Width = 305
      Height = 313
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 2
      WordWrap = False
    end
    object btnSave: TButton
      Left = 16
      Top = 453
      Width = 97
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ModalResult = 1
      ParentFont = False
      TabOrder = 3
    end
    object btnClose: TButton
      Left = 128
      Top = 453
      Width = 89
      Height = 25
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 4
    end
    object btnRefresh: TButton
      Left = 232
      Top = 453
      Width = 73
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Refresh F9'
      TabOrder = 5
      OnClick = btnRefreshClick
    end
    object btnFromCode: TButton
      Left = 256
      Top = 6
      Width = 59
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'From code'
      TabOrder = 6
      OnClick = btnFromCodeClick
    end
  end
  object PopupMenuFromCode: TPopupMenu
    TrackButton = tbLeftButton
    Left = 272
    Top = 32
    object FromCodeObjectHierarchyHide: TMenuItem
      Caption = 'Hide'
      OnClick = FromCodeClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object FromCodeObjecthierarchy: TMenuItem
      Caption = 'Object hierarchy'
    end
    object FromCodeSQLRecord: TMenuItem
      Caption = 'TSQLRecord'
    end
  end
end
