object ProjectTrackerLoginForm: TProjectTrackerLoginForm
  Left = 470
  Top = 204
  BorderStyle = bsDialog
  Caption = ' PVCS Tracker'
  ClientHeight = 345
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOk: TBitBtn
    Left = 136
    Top = 296
    Width = 105
    Height = 33
    Hint = 'OK,Connect'
    Caption = 'Connect'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = BtnOkClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000040204000482
      0400848204008402040004FE0400FCFEFC00FC02040004FEFC00000040000000
      00000000000000000000DFFBC100DA67AB00410080007E007C00000555500000
      0000002222500000000000222253633300000022225136363600002222541413
      3330002222514141363603222257143413630622225147414413552222555474
      7143222222225747471422222222577474732222222266774743006666676664
      7460006466777666436000067464666366000000024744660000}
  end
  object BtnCancel: TBitBtn
    Left = 288
    Top = 296
    Width = 105
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    Glyph.Data = {
      96010000424D9601000000000000760000002800000018000000180000000100
      04000000000020010000130B0000130B0000100000000000000004029C00FC02
      FC001343FA003461F900204FFB000424EA001C42ED000733F7002C4FE4004172
      FC00143AEC00052CF7001E48F7002D59F7003967FA002457FC00111111111111
      1111111111111111111111111111111111111111111111111111111111111111
      1001111111111001111111110270111111110BB011111110C22B01111110577B
      011111102C227011110B7B7501111111064C270110B77750111111111064CCA0
      0BBB75011111111111064CCA77B750111111111111106422A77B011111111111
      11110C2222701111111111111111044C22A01111111111111110DF44444A0111
      11111111110D3DDCCF44601111111111103E3E400CF44601111111110E9EE801
      106F446011111110999980111106D44C011111109998011111106DFC01111111
      09D0111111110C40111111111001111111111001111111111111111111111111
      1111111111111111111111111111111111111111111111111111}
  end
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 431
    Height = 281
    ActivePage = TabSCRMulti
    TabOrder = 2
    object TabUser: TTabSheet
      TabVisible = False
      object Label3: TLabel
        Left = 40
        Top = 32
        Width = 4
        Height = 14
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object UserName: TLabeledEdit
        Left = 40
        Top = 80
        Width = 353
        Height = 21
        EditLabel.Width = 175
        EditLabel.Height = 13
        EditLabel.Caption = 'User Name (leave blank for default):'
        TabOrder = 0
      end
      object PassWord: TLabeledEdit
        Left = 40
        Top = 128
        Width = 353
        Height = 21
        EditLabel.Width = 169
        EditLabel.Height = 13
        EditLabel.Caption = 'Password (leave blank for default):'
        TabOrder = 1
      end
    end
    object TabProject: TTabSheet
      ImageIndex = 1
      TabVisible = False
      object Label1: TLabel
        Left = 40
        Top = 32
        Width = 70
        Height = 13
        Caption = 'Select Project:'
      end
      object Project: TListBox
        Left = 40
        Top = 48
        Width = 345
        Height = 185
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabSCR: TTabSheet
      ImageIndex = 2
      TabVisible = False
      object Label2: TLabel
        Left = 40
        Top = 16
        Width = 113
        Height = 13
        Caption = 'Choose one SCR entry:'
      end
      object SCRList: TListBox
        Left = 40
        Top = 32
        Width = 345
        Height = 225
        Style = lbOwnerDrawFixed
        ItemHeight = 13
        TabOrder = 0
        OnClick = SCRListClick
        OnDrawItem = SCRListDrawItem
      end
    end
    object TabSCRMulti: TTabSheet
      ImageIndex = 3
      TabVisible = False
      object Label4: TLabel
        Left = 40
        Top = 16
        Width = 3
        Height = 13
      end
      object SCRListMulti: TCheckListBox
        Left = 40
        Top = 32
        Width = 345
        Height = 225
        OnClickCheck = SCRListMultiClickCheck
        ItemHeight = 16
        Style = lbOwnerDrawFixed
        TabOrder = 0
        OnDrawItem = SCRListDrawItem
      end
      object BtnSelAll: TButton
        Left = 4
        Top = 168
        Width = 33
        Height = 25
        Hint = 'Select all entries'
        Caption = 'All'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = BtnSelAllClick
      end
      object BtnSelNone: TButton
        Left = 4
        Top = 200
        Width = 33
        Height = 25
        Hint = 'Select no entry'
        Caption = 'None'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = BtnSelAllClick
      end
      object BtnSelOpen: TButton
        Left = 4
        Top = 232
        Width = 33
        Height = 25
        Hint = 'Select all opened entries'
        Caption = 'Open'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = BtnSelAllClick
      end
    end
  end
end
