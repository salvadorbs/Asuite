object FormViewTwo: TFormViewTwo
  Left = 257
  Top = 137
  Width = 522
  Height = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 265
    Top = 0
    Height = 421
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 265
    Height = 421
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object PanelRight: TPanel
    Left = 268
    Top = 0
    Width = 246
    Height = 421
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
