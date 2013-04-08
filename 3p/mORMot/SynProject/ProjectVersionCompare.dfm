object ProjectVersionCompareForm: TProjectVersionCompareForm
  Left = 162
  Top = 354
  Width = 547
  Height = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelLeft: TPanel
    Left = 225
    Top = 0
    Width = 148
    Height = 342
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object PanelRight: TPanel
    Left = 373
    Top = 0
    Width = 203
    Height = 342
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
  end
  object ListFilesCommit: TListView
    Left = 0
    Top = 0
    Width = 225
    Height = 342
    Align = alLeft
    Columns = <>
    MultiSelect = True
    TabOrder = 2
    OnSelectItem = ListFilesCommitSelectItem
  end
end
