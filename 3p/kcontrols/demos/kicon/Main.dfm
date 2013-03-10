object MainForm: TMainForm
  Left = 404
  Top = 299
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'KIcon demo'
  ClientHeight = 503
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SBIcon: TSpeedButton
    Left = 297
    Top = 434
    Width = 109
    Height = 57
  end
  object BULoad: TButton
    Left = 297
    Top = 16
    Width = 112
    Height = 25
    Caption = 'Load...'
    TabOrder = 0
    OnClick = BULoadClick
  end
  object BUSave: TButton
    Left = 297
    Top = 109
    Width = 112
    Height = 25
    Action = ACSave
    TabOrder = 1
  end
  object BUAdd: TButton
    Left = 297
    Top = 140
    Width = 112
    Height = 25
    Action = ACAddImage
    TabOrder = 2
  end
  object BURemove: TButton
    Left = 297
    Top = 171
    Width = 112
    Height = 25
    Action = ACRemoveImage
    TabOrder = 3
  end
  object BUBackground: TButton
    Left = 297
    Top = 233
    Width = 112
    Height = 25
    Caption = 'Background color...'
    TabOrder = 4
    OnClick = BUBackgroundClick
  end
  object CBStretch: TCheckBox
    Left = 297
    Top = 272
    Width = 120
    Height = 17
    Caption = 'Stretch'
    TabOrder = 5
    OnClick = CBStretchClick
  end
  object RGDrawStyle: TRadioGroup
    Left = 297
    Top = 339
    Width = 109
    Height = 89
    Caption = 'Draw Style'
    ItemIndex = 0
    Items.Strings = (
      'normal'
      'no mask'
      'mask only'
      'alpha channel')
    TabOrder = 6
    OnClick = RGDrawStyleClick
  end
  object PNMain: TPanel
    Left = 16
    Top = 16
    Width = 268
    Height = 242
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentBackground = False
    TabOrder = 7
    object IMMain: TImage
      Left = 0
      Top = 0
      Width = 264
      Height = 238
      Align = alClient
      Center = True
      ExplicitTop = 72
      ExplicitHeight = 231
    end
  end
  object CBDisplayAll: TCheckBox
    Left = 297
    Top = 296
    Width = 120
    Height = 17
    Caption = 'Display all images'
    TabOrder = 8
    OnClick = CBDisplayAllClick
  end
  object LVMain: TListView
    Left = 16
    Top = 376
    Width = 268
    Height = 115
    Columns = <
      item
        Caption = 'Image'
      end
      item
        Caption = 'Width'
      end
      item
        Caption = 'Height'
      end
      item
        Caption = 'Resolution'
        Width = 70
      end
      item
        Caption = 'PNG'
        Width = 40
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 9
    ViewStyle = vsReport
    OnSelectItem = LVMainSelectItem
  end
  object CBDisplayHorz: TCheckBox
    Left = 297
    Top = 318
    Width = 108
    Height = 17
    Action = ACDisplayHorz
    State = cbChecked
    TabOrder = 10
  end
  object BULoadMain: TButton
    Left = 297
    Top = 47
    Width = 112
    Height = 24
    Caption = 'Load main icon'
    TabOrder = 11
    OnClick = BULoadMainClick
  end
  object BUExtractBitmap: TButton
    Left = 297
    Top = 202
    Width = 112
    Height = 25
    Action = ACExtractBitmap
    Caption = 'Extract to...'
    TabOrder = 12
  end
  object BUExtract: TButton
    Left = 297
    Top = 78
    Width = 112
    Height = 25
    Caption = 'Extract from file...'
    TabOrder = 13
    OnClick = BUExtractClick
  end
  object PNAlpha: TPanel
    Left = 16
    Top = 272
    Width = 264
    Height = 89
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 14
    object PBAlpha: TPaintBox
      Left = 0
      Top = 0
      Width = 260
      Height = 85
      Align = alClient
      OnPaint = PBAlphaPaint
      ExplicitLeft = 72
      ExplicitTop = 8
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
  end
  object ODMain: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 160
    Top = 48
  end
  object SDMain: TSaveDialog
    DefaultExt = 'ico'
    Left = 192
    Top = 48
  end
  object ALMain: TActionList
    Left = 128
    Top = 48
    object ACSave: TAction
      Caption = 'Save...'
      OnExecute = ACSaveExecute
      OnUpdate = ACSaveUpdate
    end
    object ACAddImage: TAction
      Caption = 'Add image...'
      OnExecute = ACAddImageExecute
      OnUpdate = ACSaveUpdate
    end
    object ACRemoveImage: TAction
      Caption = 'Remove image'
      OnExecute = ACRemoveImageExecute
      OnUpdate = ACRemoveImageUpdate
    end
    object ACDisplayHorz: TAction
      Caption = 'Horizontally'
      Checked = True
      OnExecute = ACDisplayHorzExecute
      OnUpdate = ACDisplayHorzUpdate
    end
    object ACExtractBitmap: TAction
      Caption = 'Extract bitmap...'
      OnExecute = ACExtractBitmapExecute
      OnUpdate = ACSaveUpdate
    end
  end
  object CDMain: TColorDialog
    Left = 224
    Top = 48
  end
end
