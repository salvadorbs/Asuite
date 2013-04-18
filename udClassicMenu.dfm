object ClassicMenu: TClassicMenu
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object tiTrayMenu: TTrayIcon
    Visible = True
    OnDblClick = tiTrayMenuDblClick
    OnMouseDown = tiTrayMenuMouseDown
    Left = 16
    Top = 16
  end
  object pmTrayicon: TPopupMenu
    Left = 88
    Top = 16
  end
end
