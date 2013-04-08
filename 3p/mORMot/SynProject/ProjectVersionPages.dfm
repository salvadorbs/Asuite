object FramePages: TFramePages
  Left = 0
  Top = 0
  Width = 567
  Height = 315
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 567
    Height = 315
    ActivePage = PageFile
    Align = alClient
    TabOrder = 0
    OnChange = PagesChange
    object PageCommit: TTabSheet
      Caption = 'By Commit'
      object Splitter1: TSplitter
        Left = 201
        Top = 0
        Height = 287
      end
      object ListCommit: TListView
        Left = 0
        Top = 0
        Width = 201
        Height = 287
        Align = alLeft
        Columns = <>
        Items.Data = {1D0000000100000000000000FFFFFFFFFFFFFFFF000000000000000000}
        TabOrder = 0
      end
      object CommitRight: TPanel
        Left = 204
        Top = 0
        Width = 355
        Height = 287
        Align = alClient
        TabOrder = 1
        object ListCommitFiles: TListView
          Left = 1
          Top = 25
          Width = 353
          Height = 261
          Align = alClient
          Columns = <>
          TabOrder = 0
          OnMouseDown = ListCommitFilesMouseDown
        end
        object CommitRightTop: TPanel
          Left = 1
          Top = 1
          Width = 353
          Height = 24
          Align = alTop
          TabOrder = 1
          DesignSize = (
            353
            24)
          object LabelCommitRightTop: TLabel
            Left = 110
            Top = 6
            Width = 239
            Height = 13
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsItalic]
            ParentFont = False
          end
          object BtnComitRightTop: TSpeedButton
            Left = 5
            Top = 4
            Width = 16
            Height = 16
            Hint = 'Show / Hide Commit column'
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FF006600006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0066001EB2311FB13300
              6600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FF00660031C24F22B7381AB02D21B437006600FF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FF00660047D36D3BCB5E25A83B0066001B
              A92E1DB132006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF006600
              4FD67953DE7F31B54D006600FF00FF006600179D271EAE31006600FF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FF00660041C563006600FF00FFFF00FFFF
              00FFFF00FF00660019AA2B006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FF006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF006600149D210066
              00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FF006600006600FF00FFFF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FF006600006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
            Layout = blGlyphBottom
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
            OnClick = BtnComitRightTopClick
          end
          object CheckBoxOnlyModified: TCheckBox
            Left = 24
            Top = 5
            Width = 89
            Height = 17
            Caption = 'only modified'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CheckBoxOnlyModifiedClick
          end
        end
      end
    end
    object PageFile: TTabSheet
      Caption = 'By File'
      ImageIndex = 1
      object Splitter2: TSplitter
        Left = 329
        Top = 0
        Height = 287
      end
      object ListFiles: TListView
        Left = 0
        Top = 0
        Width = 329
        Height = 287
        Align = alLeft
        Columns = <>
        TabOrder = 0
      end
      object ListFilesCommit: TListView
        Left = 332
        Top = 0
        Width = 227
        Height = 287
        Align = alClient
        Columns = <>
        MultiSelect = True
        TabOrder = 1
        OnSelectItem = ListFilesCommitSelectItem
      end
    end
  end
end
