object dmTrayMenu: TdmTrayMenu
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object tiTrayMenu: TTrayIcon
    Visible = True
    OnDblClick = tiTrayMenuDblClick
    OnMouseDown = tiTrayMenuMouseDown
    Left = 48
    Top = 16
  end
  object pmTrayicon: TPopupMenu
    Images = dmImages.IcoImages
    Left = 16
    Top = 16
  end
end
