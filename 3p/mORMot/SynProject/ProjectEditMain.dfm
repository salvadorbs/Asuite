object ProMainForm: TProMainForm
  Left = 250
  Top = 303
  Width = 668
  Height = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 121
    Top = 0
    Height = 379
  end
  object Splitter1: TSplitter
    Left = 649
    Top = 0
    Height = 379
    Align = alRight
  end
  object Sections: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 379
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = SectionsClick
    OnMouseDown = SectionsMouseDown
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 379
    Width = 652
    Height = 19
    AutoHint = True
    Panels = <
      item
        Width = 500
      end
      item
        Width = 50
      end>
  end
end
