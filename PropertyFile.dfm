object frmPropertyFile: TfrmPropertyFile
  Left = 370
  Top = 237
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Properties'
  ClientHeight = 362
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 233
    Top = 327
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 319
    Top = 327
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 385
    Height = 313
    ActivePage = tsInfo2
    TabOrder = 0
    object tsInfo1: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbInfo1: TLabel
        Left = 9
        Top = 9
        Width = 361
        Height = 41
        AutoSize = False
        Caption = 
          'Every page in ASuite requires some information about the respect' +
          'ive application. All fields are optional except the application'#39 +
          's name.'
        Color = clBtnFace
        ParentColor = False
        WordWrap = True
      end
      object lbName: TLabel
        Left = 9
        Top = 56
        Width = 27
        Height = 13
        Caption = 'Name'
        Color = clBtnFace
        ParentColor = False
      end
      object lbPathExe: TLabel
        Left = 9
        Top = 151
        Width = 161
        Height = 13
        Caption = 'Executable/folder/web page path'
        Color = clBtnFace
        ParentColor = False
      end
      object lbParameters: TLabel
        Left = 8
        Top = 197
        Width = 104
        Height = 13
        Caption = 'Parameters (optional)'
        Color = clBtnFace
        ParentColor = False
      end
      object lbInfo2: TLabel
        Left = 8
        Top = 243
        Width = 361
        Height = 25
        AutoSize = False
        Caption = 
          'Note: You can use $ASuite even when applications are in the same' +
          ' folder as ASuite.'
        Color = clBtnFace
        ParentColor = False
        WordWrap = True
      end
      object edtName: TEdit
        Left = 9
        Top = 72
        Width = 145
        Height = 21
        TabOrder = 0
        OnEnter = edtNameEnter
      end
      object edtPathExe: TEdit
        Left = 9
        Top = 170
        Width = 289
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '$ASuite\'
        OnExit = edtPathExeExit
      end
      object edtParameters: TEdit
        Left = 8
        Top = 216
        Width = 289
        Height = 21
        TabOrder = 3
      end
      object btnBrowseExe: TButton
        Left = 304
        Top = 170
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 2
        OnClick = Browse
      end
    end
    object tsInfo2: TTabSheet
      Caption = 'Advanced'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbPathIcon: TLabel
        Left = 8
        Top = 48
        Width = 132
        Height = 13
        Caption = 'Custom icon path (optional)'
        Color = clBtnFace
        ParentColor = False
      end
      object lbAutoExecute: TLabel
        Left = 8
        Top = 88
        Width = 62
        Height = 13
        Caption = 'Autoexecute'
        Color = clBtnFace
        ParentColor = False
      end
      object lbWindowState: TLabel
        Left = 272
        Top = 176
        Width = 66
        Height = 13
        Caption = 'Window state'
        Color = clBtnFace
        ParentColor = False
      end
      object lbWorkingDir: TLabel
        Left = 8
        Top = 8
        Width = 171
        Height = 13
        Caption = 'Custom working directory (optional)'
        Color = clBtnFace
        ParentColor = False
      end
      object lbActionOnExe: TLabel
        Left = 221
        Top = 129
        Width = 64
        Height = 13
        Caption = 'On execution'
        Color = clBtnFace
        ParentColor = False
      end
      object edtPathIcon: TEdit
        Left = 8
        Top = 64
        Width = 289
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = '$ASuite\'
      end
      object btnBrowseIcon: TButton
        Left = 309
        Top = 64
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 3
        OnClick = Browse
      end
      object cxAutoExecute: TComboBox
        Left = 8
        Top = 104
        Width = 249
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = cxAutoExecuteChange
        Items.Strings = (
          'Never'
          'Always on startup'
          'Startup, only if no previous instances are running'
          'Always on shutdown')
      end
      object cxWindowState: TComboBox
        Left = 272
        Top = 192
        Width = 102
        Height = 21
        Style = csDropDownList
        TabOrder = 7
        Items.Strings = (
          'Normal'
          'Minimized'
          'Maximized')
      end
      object edtWorkingDir: TEdit
        Left = 8
        Top = 24
        Width = 289
        Height = 21
        TabOrder = 0
        Text = '$ASuite\'
      end
      object btnBrowseWorkingDir: TButton
        Left = 309
        Top = 24
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
        OnClick = Browse
      end
      object cxActionOnExe: TComboBox
        Left = 221
        Top = 148
        Width = 153
        Height = 21
        Style = csDropDownList
        TabOrder = 6
        Items.Strings = (
          'Default (options)'
          'Just run file'
          'Run and hide ASuite'
          'Run and close ASuite')
      end
      object cbDontInsertMRU: TCheckBox
        Left = 8
        Top = 219
        Width = 224
        Height = 19
        Caption = 'Don'#39't insert this software in recents (MRU)'
        TabOrder = 10
      end
      object cbShortcutDesktop: TCheckBox
        Left = 8
        Top = 250
        Width = 264
        Height = 19
        Caption = 'Create shortcut on desktop when ASuite is running'
        TabOrder = 11
      end
      object cbHideSoftware: TCheckBox
        Left = 8
        Top = 152
        Width = 161
        Height = 19
        Caption = 'Hide this software from menu'
        TabOrder = 8
      end
      object btnChangeOrder: TButton
        Left = 272
        Top = 104
        Width = 102
        Height = 21
        Caption = 'Change order'
        TabOrder = 5
        OnClick = btnChangeOrderClick
      end
      object cbDontInsertMFU: TCheckBox
        Left = 8
        Top = 188
        Width = 176
        Height = 19
        Caption = 'Don'#39't insert this software in MFU'
        TabOrder = 9
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 296
  end
end
