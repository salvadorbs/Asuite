object fMain: TfMain
  Left = 495
  Top = 318
  Width = 620
  Height = 464
  ActiveControl = eProjectFile
  Caption = 'DKLang Project Resource Converter'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    612
    435)
  PixelsPerInch = 96
  TextHeight = 13
  object lConversionLog: TLabel
    Left = 8
    Top = 196
    Width = 75
    Height = 13
    Caption = 'Conversion &log:'
    FocusControl = mConversionLog
  end
  object pMain: TPanel
    Left = 8
    Top = 8
    Width = 595
    Height = 185
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      595
      185)
    object lInfo: TLabel
      Left = 12
      Top = 12
      Width = 439
      Height = 39
      Caption = 
        'This program converts DKLang constant resources from library ver' +
        'sion 2.x:'#13#10'  - Version 2.x constants were stored inside project'#39 +
        's resource file <project_name>.res'#13#10'  - Version 3.0+ stores cons' +
        'tants in a separate resource file <project_name>.dkl_const.res'
    end
    object lProjectFile: TLabel
      Left = 12
      Top = 64
      Width = 100
      Height = 13
      Caption = '&Project resource file:'
      FocusControl = eProjectFile
    end
    object lOutputFile: TLabel
      Left = 12
      Top = 104
      Width = 100
      Height = 13
      Caption = 'O&utput resource file:'
      FocusControl = eOutputFile
    end
    object eProjectFile: TEdit
      Left = 12
      Top = 80
      Width = 491
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = eProjectFileChange
    end
    object bProjectFileBrowse: TButton
      Left = 506
      Top = 80
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Browse...'
      TabOrder = 1
      OnClick = bProjectFileBrowseClick
    end
    object eOutputFile: TEdit
      Left = 12
      Top = 120
      Width = 491
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = eOutputFileChange
    end
    object bBrowseOutputFile: TButton
      Left = 506
      Top = 120
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Bro&wse...'
      TabOrder = 3
      OnClick = bBrowseOutputFileClick
    end
    object bGo: TButton
      Left = 508
      Top = 152
      Width = 72
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Go'
      TabOrder = 5
      OnClick = bGoClick
    end
    object cbRemoveFromProject: TCheckBox
      Left = 12
      Top = 152
      Width = 253
      Height = 17
      Caption = '&Remove constant resource from the project file'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object mConversionLog: TMemo
    Left = 8
    Top = 212
    Width = 595
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object odProjectFile: TOpenDialog
    DefaultExt = 'res'
    Filter = 'Resource files (*.res)|*.res'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select project resource file'
    Left = 472
    Top = 28
  end
  object sdOutputFile: TSaveDialog
    DefaultExt = 'dkl_const.res'
    Filter = 'DKLang constant resource files (*.dkl_const.res)|*.dkl_const.res'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Select target resource file'
    Left = 540
    Top = 28
  end
end
