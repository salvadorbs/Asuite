object FrameRisk: TFrameRisk
  Left = 0
  Top = 0
  Width = 353
  Height = 171
  TabOrder = 0
  object GroupBoxRisk: TGroupBox
    Left = 0
    Top = 0
    Width = 353
    Height = 169
    Caption = ' Risk Assessment '
    TabOrder = 0
    DesignSize = (
      353
      169)
    object Label1: TLabel
      Left = 80
      Top = 32
      Width = 3
      Height = 13
      Alignment = taRightJustify
    end
    object Label2: TLabel
      Left = 80
      Top = 56
      Width = 3
      Height = 13
      Alignment = taRightJustify
    end
    object Label3: TLabel
      Left = 80
      Top = 80
      Width = 3
      Height = 13
      Alignment = taRightJustify
    end
    object ComboBox1: TComboBox
      Left = 88
      Top = 28
      Width = 245
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
    end
    object ComboBox2: TComboBox
      Left = 88
      Top = 52
      Width = 245
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
    end
    object ComboBox3: TComboBox
      Left = 88
      Top = 76
      Width = 245
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 2
    end
    object LabeledEditEvaluatedBy: TLabeledEdit
      Left = 88
      Top = 104
      Width = 245
      Height = 21
      Hint = 'Put the SW risk evaluation team names, separated with +'
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 62
      EditLabel.Height = 13
      EditLabel.Caption = 'Evaluated by'
      LabelPosition = lpLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnKeyPress = LabeledEditEvaluatedByKeyPress
    end
    object LabeledEditJustif: TLabeledEdit
      Left = 88
      Top = 130
      Width = 245
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 55
      EditLabel.Height = 13
      EditLabel.Caption = 'Justification'
      LabelPosition = lpLeft
      TabOrder = 4
    end
  end
end
