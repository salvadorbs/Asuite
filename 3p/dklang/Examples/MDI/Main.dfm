object fMain: TfMain
  Left = 438
  Top = 199
  Caption = 'Sample MDI Application'
  ClientHeight = 290
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 461
    Height = 61
    Align = alTop
    TabOrder = 0
    DesignSize = (
      461
      61)
    object cbLanguage: TComboBox
      Left = 4
      Top = 4
      Width = 453
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbLanguageChange
    end
    object bNewWindow: TButton
      Left = 4
      Top = 32
      Width = 75
      Height = 23
      Caption = '&New window'
      TabOrder = 1
      OnClick = bNewWindowClick
    end
    object bTile: TButton
      Left = 84
      Top = 32
      Width = 75
      Height = 23
      Caption = '&Tile'
      TabOrder = 2
      OnClick = bTileClick
    end
    object bCascade: TButton
      Left = 164
      Top = 32
      Width = 75
      Height = 23
      Caption = '&Cascade'
      TabOrder = 3
      OnClick = bCascadeClick
    end
  end
  object lcMain: TDKLanguageController
    Left = 412
    Top = 32
    LangData = {
      0500664D61696E010100000001000000070043617074696F6E01050000000A00
      63624C616E67756167650000040070546F7000000A00624E657757696E646F77
      010100000002000000070043617074696F6E0005006254696C65010100000003
      000000070043617074696F6E0008006243617363616465010100000004000000
      070043617074696F6E00}
  end
end
