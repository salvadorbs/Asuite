object frmScanFolder: TfrmScanFolder
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Scan folder'
  ClientHeight = 266
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    579
    266)
  PixelsPerInch = 96
  TextHeight = 13
  object btnScan: TButton
    Left = 415
    Top = 234
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Scan'
    Default = True
    TabOrder = 2
    ExplicitTop = 311
  end
  object btnCancel: TButton
    Left = 496
    Top = 234
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    ExplicitTop = 311
  end
  object pnlFilters: TPanel
    Left = 207
    Top = 8
    Width = 364
    Height = 220
    Anchors = [akTop, akRight, akBottom]
    TabOrder = 1
    ExplicitHeight = 297
    object grpFileTypes: TGroupBox
      Left = 8
      Top = 87
      Width = 169
      Height = 121
      Caption = 'File Types'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object btnTypesDelete: TButton
        Left = 103
        Top = 80
        Width = 58
        Height = 17
        Caption = 'Delete'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object btnTypesAdd: TButton
        Left = 103
        Top = 57
        Width = 58
        Height = 17
        Caption = 'Add'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object edtTypes: TEdit
        Left = 103
        Top = 30
        Width = 58
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object vstTypes: TVirtualStringTree
        Left = 9
        Top = 22
        Width = 88
        Height = 83
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Images = ilExtIcons
        ParentFont = False
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        OnGetText = vstGetText
        OnGetImageIndex = vstGetImageIndex
        OnGetNodeDataSize = vstGetNodeDataSize
        Columns = <>
      end
    end
    object grpExclude: TGroupBox
      Left = 183
      Top = 87
      Width = 169
      Height = 121
      Caption = 'Exclude file or folder'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      object edtExclude: TEdit
        Left = 103
        Top = 30
        Width = 58
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object btnExcludeAdd: TButton
        Left = 103
        Top = 57
        Width = 58
        Height = 17
        Caption = 'Add'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object btnExcludeDelete: TButton
        Left = 103
        Top = 80
        Width = 58
        Height = 17
        Caption = 'Delete'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object vstExclude: TVirtualStringTree
        Left = 9
        Top = 22
        Width = 88
        Height = 83
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Images = dmImages.ilSmallIcons
        ParentFont = False
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        OnGetText = vstGetText
        OnGetNodeDataSize = vstGetNodeDataSize
        Columns = <>
      end
    end
    object grpGeneralSettings: TGroupBox
      Left = 8
      Top = 7
      Width = 345
      Height = 74
      Caption = 'General settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object chkFlat: TCheckBox
        Left = 9
        Top = 20
        Width = 328
        Height = 17
        Caption = 'Flat structure'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object chkExtractName: TCheckBox
        Left = 9
        Top = 43
        Width = 328
        Height = 17
        Caption = 'Extract name automatically from executables (only *.exe)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
  end
  object vstShell: TVirtualExplorerTree
    Left = 8
    Top = 8
    Width = 193
    Height = 220
    Active = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColumnDetails = cdUser
    ColumnMenuItemCount = 8
    DefaultNodeHeight = 17
    DragHeight = 250
    DragWidth = 150
    FileObjects = [foFolders, foEnableAsync]
    FileSizeFormat = fsfExplorer
    FileSort = fsFileType
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 17
    Header.MainColumn = -1
    HintMode = hmHint
    ParentColor = False
    RootFolder = rfDrives
    TabOrder = 0
    TabStop = True
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toToggleOnDblClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowTreeLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toRightClickSelect]
    TreeOptions.VETFolderOptions = [toFoldersExpandable, toForceHideRecycleBin, toThreadedExpandMark]
    TreeOptions.VETShellOptions = [toRightAlignSizeColumn]
    TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
    TreeOptions.VETMiscOptions = [toChangeNotifierThread, toVETReadOnly]
    TreeOptions.VETImageOptions = [toImages, toThreadedImages]
    OnEnumFolder = vstShellEnumFolder
    OnInitNode = vstShellInitNode
    ExplicitHeight = 297
    Columns = <>
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 434
    LangData = {
      0D0066726D5363616E466F6C646572010100000001000000070043617074696F
      6E0112000000090062746E43616E63656C010100000003000000070043617074
      696F6E00070062746E5363616E010100000004000000070043617074696F6E00
      0A00706E6C46696C74657273000008007673745368656C6C00000C0067727046
      696C655479706573010100000005000000070043617074696F6E000E0062746E
      547970657344656C657465010100000007000000070043617074696F6E000B00
      62746E5479706573416464010100000008000000070043617074696F6E000800
      656474547970657300000A006772704578636C75646501010000000900000007
      0043617074696F6E000A006564744578636C75646500000D0062746E4578636C
      75646541646401010000000A000000070043617074696F6E00100062746E4578
      636C75646544656C65746501010000000C000000070043617074696F6E000800
      767374547970657300000A007673744578636C75646500000A00696C45787449
      636F6E730000120067727047656E6572616C53657474696E677301010000000D
      000000070043617074696F6E00070063686B466C617401010000000E00000007
      0043617074696F6E000E0063686B457874726163744E616D6501010000000F00
      0000070043617074696F6E00}
  end
  object ilExtIcons: TImageList
    ColorDepth = cd32Bit
    Left = 511
    Top = 232
  end
end
