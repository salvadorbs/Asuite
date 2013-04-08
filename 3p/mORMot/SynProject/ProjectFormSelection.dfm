object SelectionForm: TSelectionForm
  Left = 352
  Top = 156
  Width = 571
  Height = 665
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object List: TListBox
    Left = 0
    Top = 25
    Width = 555
    Height = 602
    Style = lbVirtual
    Align = alClient
    ItemHeight = 18
    TabOrder = 1
    OnData = ListData
    OnDblClick = ListDblClick
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 555
    Height = 25
    Align = alTop
    TabOrder = 0
    DesignSize = (
      555
      25)
    object edtFind: TEdit
      Left = 0
      Top = 0
      Width = 530
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 0
      OnChange = edtFindChange
    end
    object btnNext: TButton
      Left = 528
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Next (F3)'
      Anchors = [akTop, akRight]
      Caption = '>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = edtFindChange
    end
  end
end
