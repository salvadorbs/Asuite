object fMain: TfMain
  Left = 321
  Top = 170
  Caption = '<>'
  ClientHeight = 430
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TheStatusBar: TStatusBar
    Left = 0
    Top = 411
    Width = 632
    Height = 19
    AutoHint = True
    Panels = <>
    SimplePanel = True
    ExplicitTop = 396
  end
  object mMain: TMemo
    Left = 0
    Top = 0
    Width = 632
    Height = 411
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnChange = UpdateStateNotify
    ExplicitHeight = 396
  end
  object mmMain: TMainMenu
    Left = 108
    Top = 32
    object smFile: TMenuItem
      Caption = '&File'
      object iFileNew: TMenuItem
        Action = aFileNew
      end
      object iFileOpen: TMenuItem
        Action = aFileOpen
      end
      object iFileSave: TMenuItem
        Action = aFileSave
      end
      object iFileSaveAs: TMenuItem
        Action = aFileSaveAs
      end
      object iSepFileExit: TMenuItem
        Caption = '-'
      end
      object iFileExit: TMenuItem
        Action = aFileExit
      end
    end
    object smEdit: TMenuItem
      Caption = '&Edit'
      object iEditUndo: TMenuItem
        Action = aEditUndo
      end
      object iSepEditCut: TMenuItem
        Caption = '-'
      end
      object iEditCut: TMenuItem
        Action = aEditCut
      end
      object iEditCopy: TMenuItem
        Action = aEditCopy
      end
      object iEditPaste: TMenuItem
        Action = aEditPaste
      end
      object iSepEditFind: TMenuItem
        Caption = '-'
      end
      object iEditFind: TMenuItem
        Action = aEditFind
      end
      object iEditFindNext: TMenuItem
        Action = aEditFindNext
      end
      object iEditReplace: TMenuItem
        Action = aEditReplace
      end
      object iEditGoToLine: TMenuItem
        Action = aEditGoToLine
      end
      object iSepEditSelectAll: TMenuItem
        Caption = '-'
      end
      object iEditSelectAll: TMenuItem
        Action = aEditSelectAll
      end
      object iEditDateAndTime: TMenuItem
        Action = aEditDateAndTime
      end
    end
    object smFormat: TMenuItem
      Caption = 'Fo&rmat'
      object iFormatWordWrap: TMenuItem
        Action = aFormatWordWrap
      end
      object iFormatFont: TMenuItem
        Action = aFormatFont
      end
    end
    object smView: TMenuItem
      Caption = '&View'
      object iViewStatusBar: TMenuItem
        Action = aViewStatusBar
      end
      object iSepViewLanguage: TMenuItem
        Caption = '-'
      end
      object smViewLanguage: TMenuItem
        Caption = '&Language'
      end
    end
    object smHelp: TMenuItem
      Caption = '&Help'
      object iHelpAbout: TMenuItem
        Action = aHelpAbout
      end
    end
  end
  object alMain: TActionList
    Left = 68
    Top = 32
    object aFileNew: TAction
      Category = 'File'
      Caption = '&New'
      Hint = 'New|Clear editor contents'
      ShortCut = 16462
      OnExecute = aFileNewExecute
    end
    object aFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open...|Open an existing file'
      ShortCut = 16463
      OnExecute = aFileOpenExecute
    end
    object aFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      Hint = 'Save|Save the text into the current file'
      ShortCut = 16467
      OnExecute = aFileSaveExecute
    end
    object aFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save &as...'
      Hint = 'Save as...|Save the text into another file'
      ShortCut = 123
      OnExecute = aFileSaveAsExecute
    end
    object aFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Exit the program'
      OnExecute = aFileExitExecute
    end
    object aEditUndo: TAction
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Undo or redo the last change'
      ShortCut = 16474
      SecondaryShortCuts.Strings = (
        'Alt+Backspace')
      OnExecute = aEditUndoExecute
    end
    object aEditCut: TAction
      Category = 'Edit'
      Caption = '&Cut'
      Hint = 'Cut|Cut selected text into the clipboard'
      ShortCut = 16472
      SecondaryShortCuts.Strings = (
        'Shift+Del')
      OnExecute = aEditCutExecute
    end
    object aEditCopy: TAction
      Category = 'Edit'
      Caption = 'Cop&y'
      Hint = 'Copy|Copy selected text into the clipboard'
      ShortCut = 16451
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = aEditCopyExecute
    end
    object aEditPaste: TAction
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Paste text from the clipboard'
      ShortCut = 16470
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
      OnExecute = aEditPasteExecute
    end
    object aEditFind: TAction
      Category = 'Edit'
      Caption = '&Find...'
      Hint = 'Find...|Open Find text dialog'
      ShortCut = 16454
      OnExecute = aEditFindExecute
    end
    object aEditFindNext: TAction
      Category = 'Edit'
      Caption = 'Find &next'
      Hint = 'Find next|Repeat the last search'
      ShortCut = 114
      OnExecute = aEditFindNextExecute
    end
    object aEditReplace: TAction
      Category = 'Edit'
      Caption = '&Replace...'
      Hint = 'Replace...|Open Replace text dialog'
      ShortCut = 16456
      OnExecute = aEditReplaceExecute
    end
    object aEditGoToLine: TAction
      Category = 'Edit'
      Caption = '&Go to line...'
      Hint = 'Go to line...|Locate a particular text line'
      ShortCut = 16455
      OnExecute = aEditGoToLineExecute
    end
    object aEditSelectAll: TAction
      Category = 'Edit'
      Caption = '&Select all'
      Hint = 'Select all|Select the whole text'
      ShortCut = 16449
      OnExecute = aEditSelectAllExecute
    end
    object aEditDateAndTime: TAction
      Category = 'Edit'
      Caption = '&Date and time'
      Hint = 'Date and time|Paste current date and time'
      ShortCut = 116
      OnExecute = aEditDateAndTimeExecute
    end
    object aFormatWordWrap: TAction
      Category = 'Format'
      Caption = '&Word wrap'
      Hint = 'Word wrap|Toggle word wrap'
      OnExecute = aFormatWordWrapExecute
    end
    object aFormatFont: TAction
      Category = 'Format'
      Caption = '&Font...'
      Hint = 'Font...|Select editor font'
      OnExecute = aFormatFontExecute
    end
    object aViewStatusBar: TAction
      Category = 'View'
      Caption = '&Status bar'
      Hint = 'Status bar|Toggle the status bar'
      OnExecute = aViewStatusBarExecute
    end
    object aHelpAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      Hint = 'About...|Show version and copyright info'
      ShortCut = 16496
      OnExecute = aHelpAboutExecute
    end
  end
  object odMain: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt;*.log)|*.txt;*.log|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a file to open'
    Left = 152
    Top = 32
  end
  object sdMain: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'ANSI text files (*.txt)|*.txt|Unicode text files (*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Select a file to save to'
    Left = 192
    Top = 32
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.DefaultExt'
      '*.SecondaryShortCuts')
    Left = 28
    Top = 32
    LangData = {
      0500664D61696E0001380000000C00546865537461747573426172000006006D
      6D4D61696E00000600736D46696C65010100000002000000070043617074696F
      6E0008006946696C654E6577000009006946696C654F70656E00000900694669
      6C655361766500000B006946696C6553617665417300000C006953657046696C
      6545786974000009006946696C654578697400000600736D4564697401010000
      0003000000070043617074696F6E0009006945646974556E646F00000B006953
      657045646974437574000008006945646974437574000009006945646974436F
      707900000A006945646974506173746500000C00695365704564697446696E64
      00000900694564697446696E6400000D00694564697446696E644E6578740000
      0C0069456469745265706C61636500000D006945646974476F546F4C696E6500
      001100695365704564697453656C656374416C6C00000E00694564697453656C
      656374416C6C00001000694564697444617465416E6454696D6500000800736D
      466F726D6174010100000004000000070043617074696F6E000F0069466F726D
      6174576F72645772617000000B0069466F726D6174466F6E7400000600736D56
      696577010100000005000000070043617074696F6E000E006956696577537461
      74757342617200000600736D48656C7001010000000600000007004361707469
      6F6E000A006948656C7041626F757400000600616C4D61696E00000800614669
      6C654E6577010300000008000000070043617074696F6E070000000800436174
      65676F727909000000040048696E740009006146696C654F70656E0103000000
      0B000000070043617074696F6E0A000000080043617465676F72790C00000004
      0048696E740009006146696C655361766501030000000E000000070043617074
      696F6E0D000000080043617465676F72790F000000040048696E74000B006146
      696C65536176654173010300000011000000070043617074696F6E1000000008
      0043617465676F727912000000040048696E740009006146696C654578697401
      0300000014000000070043617074696F6E13000000080043617465676F727915
      000000040048696E740009006145646974556E646F0103000000170000000700
      43617074696F6E16000000080043617465676F727918000000040048696E7400
      0800614564697443757401030000001B000000070043617074696F6E1A000000
      080043617465676F72791C000000040048696E740009006145646974436F7079
      01030000001F000000070043617074696F6E1E000000080043617465676F7279
      20000000040048696E74000A0061456469745061737465010300000023000000
      070043617074696F6E22000000080043617465676F727924000000040048696E
      74000900614564697446696E64010300000027000000070043617074696F6E26
      000000080043617465676F727928000000040048696E74000D00614564697446
      696E644E65787401030000002A000000070043617074696F6E29000000080043
      617465676F72792B000000040048696E74000C0061456469745265706C616365
      01030000002D000000070043617074696F6E2C000000080043617465676F7279
      2E000000040048696E74000D006145646974476F546F4C696E65010300000030
      000000070043617074696F6E2F000000080043617465676F7279310000000400
      48696E74000E00614564697453656C656374416C6C0103000000330000000700
      43617074696F6E32000000080043617465676F727934000000040048696E7400
      1000614564697444617465416E6454696D650103000000360000000700436170
      74696F6E35000000080043617465676F727937000000040048696E74000F0061
      466F726D6174576F726457726170010300000039000000070043617074696F6E
      38000000080043617465676F72793A000000040048696E74000B0061466F726D
      6174466F6E7401030000003C000000070043617074696F6E3B00000008004361
      7465676F72793D000000040048696E74000E0061566965775374617475734261
      7201030000003F000000070043617074696F6E3E000000080043617465676F72
      7940000000040048696E74000A006148656C7041626F75740103000000420000
      00070043617074696F6E41000000080043617465676F72794300000004004869
      6E740005006D4D61696E000006006F644D61696E010200000045000000060046
      696C7465724600000005005469746C6500060073644D61696E01020000004800
      0000060046696C7465724900000005005469746C650010006953657056696577
      4C616E677561676500000E00736D566965774C616E677561676501010000004A
      000000070043617074696F6E00060066644D61696E0000}
  end
  object fdMain: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 232
    Top = 32
  end
end
