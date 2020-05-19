object frmShortcutGrabber: TfrmShortcutGrabber
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Choose hotkey...'
  ClientHeight = 189
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    396
    189)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDialogPage: TPanel
    Left = 8
    Top = 8
    Width = 380
    Height = 142
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object btnAlt: TcySkinButton
      Left = 207
      Top = 16
      Width = 74
      Height = 48
      AllowAllUp = True
      GroupIndex = 3
      Stretch = True
    end
    object btnCtrl: TcySkinButton
      Left = 15
      Top = 16
      Width = 74
      Height = 48
      AllowAllUp = True
      GroupIndex = 1
      Stretch = True
    end
    object btnShift: TcySkinButton
      Left = 111
      Top = 16
      Width = 74
      Height = 48
      AllowAllUp = True
      GroupIndex = 2
      Stretch = True
    end
    object btnWinKey: TcySkinButton
      Left = 305
      Top = 16
      Width = 46
      Height = 48
      AllowAllUp = True
      GroupIndex = 4
      Stretch = True
    end
    object lblInfo: TLabel
      Left = 15
      Top = 80
      Width = 228
      Height = 13
      Caption = 'Select modifiers above, then enter desired key:'
    end
    object hkKeys: THotKey
      Left = 122
      Top = 101
      Width = 121
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone]
      Modifiers = []
      TabOrder = 0
      OnChange = hkKeysChange
    end
  end
  object btnOk: TButton
    Left = 232
    Top = 156
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 313
    Top = 156
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object HotKeyManager1: THotKeyManager
    Left = 248
    Top = 120
  end
end
