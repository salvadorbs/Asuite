object frmUpdate: TfrmUpdate
  Left = 0
  Top = 0
  Caption = 'Update'
  ClientHeight = 178
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbDownloadInfo: TGroupBox
    Left = 8
    Top = 56
    Width = 289
    Height = 57
    Caption = 'Download info'
    TabOrder = 1
    object lbTotalSize: TLabel
      Left = 160
      Top = 16
      Width = 45
      Height = 13
      Caption = 'Total size'
    end
    object lbSpeed: TLabel
      Left = 8
      Top = 32
      Width = 30
      Height = 13
      Caption = 'Speed'
    end
    object lbBytesReceived: TLabel
      Left = 160
      Top = 32
      Width = 71
      Height = 13
      Caption = 'Bytes received'
    end
    object lbSpeed2: TLabel
      Left = 88
      Top = 32
      Width = 30
      Height = 13
      Caption = '0 KB/s'
    end
    object lbBytesReceived2: TLabel
      Left = 256
      Top = 32
      Width = 21
      Height = 13
      Caption = '0 KB'
    end
    object lbTotalSize2: TLabel
      Left = 256
      Top = 16
      Width = 21
      Height = 13
      Caption = '0 KB'
    end
    object lbFileName: TLabel
      Left = 8
      Top = 16
      Width = 45
      Height = 13
      Caption = 'File name'
    end
    object lbFileName2: TLabel
      Left = 88
      Top = 16
      Width = 51
      Height = 13
      Caption = 'xyz12.exe'
    end
  end
  object gbServerInfo: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 41
    Caption = 'Server info'
    TabOrder = 0
    object lbServerInfo: TLabel
      Left = 8
      Top = 16
      Width = 73
      Height = 13
      Caption = 'Download file x'
    end
  end
  object pbDownload: TProgressBar
    Left = 8
    Top = 120
    Width = 289
    Height = 17
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 120
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 8
    Top = 144
  end
end
