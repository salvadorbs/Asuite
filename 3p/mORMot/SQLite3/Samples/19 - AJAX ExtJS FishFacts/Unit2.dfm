object Form1: TForm1
  Left = 198
  Top = 124
  Width = 344
  Height = 201
  Caption = 'mORMot FishFacts HTTP Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 297
    Height = 33
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 88
    Top = 72
    Width = 145
    Height = 16
    Caption = 'HTTP Server is running...'
  end
  object btnQuit: TButton
    Left = 120
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Quit'
    TabOrder = 0
    OnClick = btnQuitClick
  end
  object btnShowLogs: TButton
    Left = 224
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Show Logs'
    TabOrder = 1
    OnClick = btnShowLogsClick
  end
end
