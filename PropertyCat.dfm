object frmPropertyCat: TfrmPropertyCat
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Properties'
  ClientHeight = 263
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
  object btnCancel: TButton
    Left = 319
    Top = 230
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 238
    Top = 230
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object PageControl1: TPageControl
    Left = 9
    Top = 8
    Width = 385
    Height = 216
    ActivePage = tsInfo2
    TabOrder = 0
    object tsInfo1: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbName: TLabel
        Left = 3
        Top = 5
        Width = 27
        Height = 13
        Caption = 'Name'
        Color = clBtnFace
        ParentColor = False
      end
      object lblListItems: TLabel
        Left = 3
        Top = 51
        Width = 121
        Height = 13
        Caption = 'List items in this category'
      end
      object lblNote: TLabel
        Left = 3
        Top = 151
        Width = 360
        Height = 26
        Caption = 
          'Note: In this list you can find all software items of this categ' +
          'ory (only first level). Uncheck items who you don'#39't want to star' +
          't, when run this category.'
        WordWrap = True
      end
      object edtName: TEdit
        Left = 3
        Top = 24
        Width = 145
        Height = 21
        TabOrder = 0
        OnEnter = edtNameEnter
      end
      object vstCategoryItems: TVirtualStringTree
        Left = 3
        Top = 70
        Width = 371
        Height = 77
        DragOperations = []
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Images = ImagesDM.IcoImages
        TabOrder = 1
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
        OnGetText = vstCategoryItemsGetText
        OnGetImageIndex = vstCategoryItemsGetImageIndex
        Columns = <>
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
        Top = 8
        Width = 132
        Height = 13
        Caption = 'Custom icon path (optional)'
        Color = clBtnFace
        ParentColor = False
      end
      object lbAutoExecute: TLabel
        Left = 8
        Top = 48
        Width = 62
        Height = 13
        Caption = 'Autoexecute'
        Color = clBtnFace
        ParentColor = False
      end
      object lbWindowState: TLabel
        Left = 8
        Top = 91
        Width = 66
        Height = 13
        Caption = 'Window state'
        Color = clBtnFace
        ParentColor = False
      end
      object lbActionOnExe: TLabel
        Left = 221
        Top = 88
        Width = 64
        Height = 13
        Caption = 'On execution'
        Color = clBtnFace
        ParentColor = False
      end
      object edtPathIcon: TEdit
        Left = 8
        Top = 24
        Width = 289
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '$ASuite\'
      end
      object btnBrowseIcon: TButton
        Left = 309
        Top = 24
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
      end
      object cxAutoExecute: TComboBox
        Left = 8
        Top = 64
        Width = 249
        Height = 21
        Style = csDropDownList
        TabOrder = 2
        Items.Strings = (
          'Never'
          'Always on startup'
          'Startup, only if no previous instances are running'
          'Always on shutdown')
      end
      object cxWindowState: TComboBox
        Left = 8
        Top = 107
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        Items.Strings = (
          'Default (item'#39's settings)'
          'Normal'
          'Minimized'
          'Maximized')
      end
      object cxActionOnExe: TComboBox
        Left = 221
        Top = 107
        Width = 153
        Height = 21
        Style = csDropDownList
        TabOrder = 5
        Items.Strings = (
          'Default (options)'
          'Just run file'
          'Run and hide ASuite'
          'Run and close ASuite')
      end
      object cbHideSoftware: TCheckBox
        Left = 8
        Top = 134
        Width = 161
        Height = 19
        Caption = 'Hide this software from menu'
        TabOrder = 6
      end
      object btnChangeOrder: TButton
        Left = 272
        Top = 64
        Width = 102
        Height = 21
        Caption = 'Change order'
        TabOrder = 3
        OnClick = btnChangeOrderClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 248
    Top = 8
  end
end
