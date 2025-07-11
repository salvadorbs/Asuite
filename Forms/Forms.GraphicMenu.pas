{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Forms.GraphicMenu;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, Classes, Forms, StdCtrls, ExtCtrls, ComCtrls, Controls,
  Graphics, Dialogs, SysUtils, VirtualTrees, Menus, Lists.Base, BCImageTab,
  ButtonedEdit, BCImageButton, BCRoundedImage, GraphicMenu.ThemeEngine;

type

  { TfrmGraphicMenu }

  TfrmGraphicMenu = class(TForm)
    edtSearch: TButtonedEdit;
    imgDriveSpace: TImage;
    imgDivider2: TImage;
    imgLogo: TBCRoundedImage;
    lblDriveName: TLabel;
    lblDriveSpace: TLabel;
    sknbtnAbout: TBCImageButton;
    sknbtnASuite: TBCImageButton;
    sknbtnDocuments: TBCImageButton;
    sknbtnEject: TBCImageButton;
    sknbtnExit: TBCImageButton;
    sknbtnExplore: TBCImageButton;
    sknbtnList: TBCImageTab;
    sknbtnMFU: TBCImageTab;
    sknbtnMusic: TBCImageButton;
    sknbtnOptions: TBCImageButton;
    sknbtnPictures: TBCImageButton;
    sknbtnRecents: TBCImageTab;
    sknbtnVideos: TBCImageButton;
    tmrFader: TTimer;
    imgPersonalPicture: TImage;
    vstList: TVirtualStringTree;
    OpenDialog1: TOpenDialog;
    imgDivider1: TImage;
    ApplicationEvents1: TApplicationProperties;
    imgBackground: TImage;
    imgDriveBackground: TImage;
    pmWindow: TPopupMenu;
    mniRun: TMenuItem;
    mniRunAs: TMenuItem;
    mniRunAsAdmin: TMenuItem;
    mniOpenFolderSw: TMenuItem;
    N6: TMenuItem;
    mniProperty: TMenuItem;
    
    imgUserFrame: TImage;
    imgDragSpaceHidden: TImage;
    tmrCheckItems: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgDragSpaceHiddenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tmrFaderTimer(Sender: TObject);
    procedure imgLogoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenRightButton(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sknbtnListClick(Sender: TObject);
    procedure sknbtnRecentsClick(Sender: TObject);
    procedure sknbtnMFUClick(Sender: TObject);
    procedure sknbtnEjectClick(Sender: TObject);
    procedure sknbtnExitClick(Sender: TObject);
    procedure imgPersonalPictureClick(Sender: TObject);
    procedure mniPropertyClick(Sender: TObject);
    procedure edtSearchRightButtonClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pmWindowPopup(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure tmrCheckItemsTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure mniRunClick(Sender: TObject);
  private
    { Private declarations }
    FOpening : Boolean;
    FThemeEngine: TThemeEngine;

    procedure OpenFolder(FolderPath: string);
    procedure UpdateDriveStats;
    procedure CheckUserPicture;

    procedure PopulateListTree(const ATree: TVirtualStringTree);
    procedure PopulateSpecialTree(const ATree: TVirtualStringTree; AList: TBaseItemsList; MaxItems: Integer);
    procedure SavePositionForm;
    procedure HandleEdge(var AEdge: Integer; ASnapToEdge: Integer; ASnapDistance: Integer = 0);
  public
    { Public declarations }
    procedure OpenMenu;
    procedure CloseMenu;
    procedure LoadTheme;
  end;

var
  frmGraphicMenu : TfrmGraphicMenu;
  OldWindowX, OldWindowY: Integer;

implementation

{$R *.lfm}

uses
  Forms.Main, Utility.System, Kernel.Consts, AppConfig.Main, DataModules.Icons,
  Forms.About, NodeDataTypes.Base, Kernel.Enumerations, Forms.Options, {%H-}LazVersion,
  Utility.Misc, VirtualTree.Methods, Kernel.Types, VirtualTrees.Types,
  NodeDataTypes.Custom, Kernel.ResourceStrings, Kernel.Instance, Kernel.Manager
  {$IFDEF MSWINDOWS} , Windows {$ENDIF};

procedure TfrmGraphicMenu.ApplicationEvents1Deactivate(Sender: TObject);
begin
  //Check if GraphicMenu is not in opening state
  if Not(tmrFader.Enabled) then
    CloseMenu;
end;

procedure TfrmGraphicMenu.edtSearchChange(Sender: TObject);
var
  Node: PVirtualNode;
begin
  //Clear vstList
  vstList.Clear;
  vstList.BeginUpdate;
  try
    if edtSearch.Text <> '' then
    begin
      edtSearch.RightButton.ImageIndex := FThemeEngine.CancelIcon;

      //Do search
      //Change node height and imagelist
      TVirtualTreeMethods.ChangeTreeIconSize(vstList, False);
      frmMain.DoSearchItem(vstList, edtSearch.Text, TSearchType(GetCheckedMenuItem(frmMain.pmSearch).Tag));
      vstList.SortTree(-1, sdAscending);

      //Set first node as HotNode
      Node := vstList.GetFirst;
      if Assigned(Node) then
        vstList.HotNode := Node;
    end
    else begin
      edtSearch.RightButton.ImageIndex := FThemeEngine.SearchIcon;

      //Change node height and imagelist
      TVirtualTreeMethods.ChangeTreeIconSize(vstList, Config.GMSmallIconSize);
      PopulateListTree(vstList);
    end;
  finally
    vstList.EndUpdate;
  end;
end;

procedure TfrmGraphicMenu.edtSearchRightButtonClick(Sender: TObject);
begin
  edtSearch.Text := '';
end;

procedure TfrmGraphicMenu.CloseMenu;
begin
  //Fade in out
  FOpening := False;
  tmrFader.Enabled:= True;
end;

procedure TfrmGraphicMenu.LoadTheme;
begin
  FThemeEngine.LoadTheme;
end;

procedure TfrmGraphicMenu.SavePositionForm;
begin
  if (Self.Top <> Config.GMPositionTop) or (Self.Left <> Config.GMPositionLeft) then
  begin
    Config.GMPositionTop := Self.Top;
    Config.GMPositionLeft := Self.Left;

    Config.Changed := True;
  end;
end;

procedure TfrmGraphicMenu.HandleEdge(var AEdge: Integer; ASnapToEdge: Integer;
  ASnapDistance: Integer);
begin
  if (Abs(AEdge + ASnapDistance - ASnapToEdge) < 10) then
    AEdge := ASnapToEdge - ASnapDistance;
end;

procedure TfrmGraphicMenu.CheckUserPicture;
var
  sTempPath: string;
begin
  //User Picture
  if (Config.GMPersonalPicture = 'PersonalPicture.png') and (not FileExists(Config.GMPersonalPicture)) then
    sTempPath := ASuiteInstance.Paths.SuitePathCurrentTheme + Config.GMPersonalPicture
  else begin
    sTempPath := ASuiteInstance.Paths.RelativeToAbsolute(Config.GMPersonalPicture);
    if Not FileExists(sTempPath) then
      sTempPath := ASuiteInstance.Paths.SuitePathCurrentTheme + 'PersonalPicture.png';
  end;

  if FileExists(sTempPath) then
    imgPersonalPicture.Picture.LoadFromFile(sTempPath);

  imgPersonalPicture.Visible := (FileExists(sTempPath));
  imgUserFrame.Visible := (FileExists(sTempPath));
end;

procedure TfrmGraphicMenu.UpdateDriveStats;
var
  Drive: String;
  intDiskSize, intDiskFree, intDiskUsed: Int64;
begin
  //Calculate and display the drive size
  Drive := ASuiteInstance.Paths.SuiteDrive;

  if Length(Drive) > 0 then
  begin
    Utility.Misc.GetDiskFreeSpace(Drive, intDiskFree, intDiskSize);
    intDiskUsed := (intDiskSize - intDiskFree);
    imgDriveSpace.Width := Round(intDiskUsed  / intDiskSize * (imgDriveBackground.Width - 4));
    lblDriveSpace.Caption := Format(msgGMHardDiskSpace, [DiskFloatToString(intDiskFree, True), DiskFloatToString(intDiskUsed, True)]);
  end;
end;

procedure TfrmGraphicMenu.OpenFolder(FolderPath: string);
var
  Result: Boolean;
  sPath: string;
begin
  sPath := ASuiteInstance.Paths.RelativeToAbsolute(FolderPath);

  Result := OpenDocument(sPath);

  if not Result then
    ShowMessageFmtEx(msgErrorOpenFolder, [sPath], True);
end;

procedure TfrmGraphicMenu.FormCreate(Sender: TObject);
begin
  ASuiteInstance.VSTEvents.SetupVSTGraphicMenu(vstList, Self);

  //Load graphics
  FThemeEngine := TThemeEngine.Create(Self);
  FThemeEngine.LoadTheme;

  //Set PopUpMenu's ImageIndexes
  pmWindow.Images := dmImages.ilIcons;
  pmWindow.ImagesWidth := ICON_SIZE_SMALL;

  mniRun.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('run');
  mniProperty.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('property');

  //Position
  {$IF laz_fullversion>=2020000}
  if Config.GMPositionTop <> -1 then
    Self.Top  := Config.GMPositionTop
  else
    Self.Top  := Screen.WorkAreaRect.Bottom - Self.Scale96ToScreen(Height);
  if Config.GMPositionLeft <> -1 then
    Self.Left  := Config.GMPositionLeft
  else
    Self.Left  := Screen.WorkAreaRect.Right - Self.Scale96ToScreen(Width);
  {$ENDIF}

  edtSearch.RightButton.Images := dmImages.ilIcons;
  edtSearch.RightButton.ImagesWidth := ICON_SIZE_SMALL;
  edtSearch.RightButton.ImageIndex := FThemeEngine.SearchIcon;
end;

procedure TfrmGraphicMenu.FormDestroy(Sender: TObject);
begin
  FThemeEngine.Free;
end;

procedure TfrmGraphicMenu.imgDragSpaceHiddenMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SavePositionForm;
end;

procedure TfrmGraphicMenu.imgLogoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  CursorPos: TPoint;
  xx, yy: LongInt;
begin
  CursorPos := Mouse.CursorPos;
  //Only when user uses left click
  if ssLeft in Shift then
  begin
    if (Parent = nil) and
      ((CursorPos.X <> OldWindowX) or (CursorPos.Y <> OldWindowY)) and
      ((CursorPos.X <> 0) or (CursorPos.Y <> 0)) then
    begin
      xx := CursorPos.x - OldWindowX;
      yy := CursorPos.y - OldWindowY;

      HandleEdge(xx, Monitor.WorkareaRect.Left, 0);
      HandleEdge(yy, Monitor.WorkareaRect.Top, 0);
      HandleEdge(xx, Monitor.WorkareaRect.Right, Self.Width);
      HandleEdge(yy, Monitor.WorkareaRect.Bottom, Self.Height);

      Self.Left := xx;
      Self.Top  := yy;
    end;
  end;
end;

procedure TfrmGraphicMenu.FormDeactivate(Sender: TObject);
begin
  //if menu lost its focus, it hide
  CloseMenu;
  //Save position
  SavePositionForm;
end;

procedure TfrmGraphicMenu.FormHide(Sender: TObject);
begin
  tmrCheckItems.Enabled := False;
end;

procedure TfrmGraphicMenu.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CurrentNode: PVirtualNode;
  NodeData: TvBaseNodeData;
begin
  CurrentNode := vstList.HotNode;
  case Ord(Key) of
    VK_UP:
      begin
        Key := 0;
        CurrentNode := vstList.GetPreviousVisible(vstList.HotNode);
        if Not Assigned(CurrentNode) then
          CurrentNode := vstList.GetLast;
      end;
    VK_DOWN:
      begin
        Key := 0;
        CurrentNode := vstList.GetNextVisible(vstList.HotNode);
        if Not Assigned(CurrentNode) then
          CurrentNode := vstList.GetFirst;
      end;
    VK_RETURN:
      begin
        Key := 0;
        if Assigned(CurrentNode) then
        begin
          NodeData := TVirtualTreeMethods.GetNodeItemData(CurrentNode, vstList);
          if Assigned(NodeData) then
          begin
            case NodeData.DataType of
              vtdtCategory:
                begin
                  if (ssCtrl in Shift) then
                    TVirtualTreeMethods.ExecuteNode(vstList, CurrentNode, rmNormal, False)
                  else
                    vstList.Expanded[CurrentNode] := Not(vstList.Expanded[CurrentNode]);
                end;
              vtdtFile: TVirtualTreeMethods.ExecuteNode(vstList, CurrentNode, rmNormal, False);
            end;
          end;
        end;
      end;
    VK_LEFT:
      begin
        if Assigned(CurrentNode) then
          vstList.Expanded[CurrentNode] := False;
      end;
    VK_RIGHT:
      begin
        if Assigned(CurrentNode) then
          vstList.Expanded[CurrentNode] := True;
      end;
    VK_ESCAPE:
      begin
        CloseMenu;
      end;
  end;

  if Assigned(CurrentNode) then
    vstList.HotNode := CurrentNode;
end;

procedure TfrmGraphicMenu.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = (VK_RETURN) then
    Key := #0;
end;

procedure TfrmGraphicMenu.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //Scroll vstList using WheelDelta
  vstList.OffsetY := vstList.OffsetY + WheelDelta;
end;

procedure TfrmGraphicMenu.FormShow(Sender: TObject);
begin
  CheckUserPicture;
  imgPersonalPicture.Visible := Config.GMShowUserPicture;
  imgUserFrame.Visible := Config.GMShowUserPicture;
  sknbtnList.Pressed := True;

  //Clear edtSearch
  edtSearch.Text := '';

  //Change node height and imagelist
  TVirtualTreeMethods.ChangeTreeIconSize(vstList, Config.GMSmallIconSize);

  //Clear and populate virtualtree
  PopulateListTree(vstList);
  UpdateDriveStats;

  //Enable or disable tabs
  sknbtnRecents.Visible := Config.MRU;
  sknbtnMFU.Visible := Config.MFU;

  //Timer
  tmrCheckItems.Enabled := True;

  //TODO win32: Disable eject button if asuite runs on normal hd and not usb pen (ejectable devices)
  //Eject button visibility (in Linux always not visible, in Windows user can choose if hiding it or not)
  sknbtnEject.Visible := not(Config.GMHideEjectButton);
  {$IFDEF UNIX}
  sknbtnEject.Visible := False;
  {$ENDIF}
end;

procedure TfrmGraphicMenu.imgLogoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CursorPos: TPoint;
begin
  CursorPos := Mouse.CursorPos;
  OldWindowX := CursorPos.x - Self.Left;
  OldWindowY := CursorPos.y - Self.Top;
end;

procedure TfrmGraphicMenu.imgPersonalPictureClick(Sender: TObject);
var
  TempString : string;
begin
  TempString := '';
  OpenDialog1.Filter     := msgFilterPicture;
  OpenDialog1.InitialDir := ExtractFileDir(ASuiteInstance.Paths.RelativeToAbsolute(Config.GMPersonalPicture));
  if OpenDialog1.Execute then
  begin
    TempString := OpenDialog1.FileName;
    imgPersonalPicture.Picture.LoadFromFile(TempString);
    Config.GMPersonalPicture := ASuiteInstance.Paths.AbsoluteToRelative(TempString);
    Config.Changed := True;
  end;
end;

procedure TfrmGraphicMenu.mniPropertyClick(Sender: TObject);
begin
  TVirtualTreeMethods.ShowItemProperty(Self, vstList, vstList.GetFirstSelected);
end;

procedure TfrmGraphicMenu.mniRunClick(Sender: TObject);
begin
  TVirtualTreeMethods.ExecuteSelectedNodes(vstList, TRunMode(TMenuItem(Sender).Tag), False);
end;

procedure TfrmGraphicMenu.OpenRightButton(Sender: TObject);
begin
  if (Sender is TBCImageButton) then
  begin
    //Close Graphic Menu
    frmGraphicMenu.CloseMenu;
    if (Sender = sknbtnASuite) then
    begin
      frmMain.ShowMainForm(Sender);
      frmMain.pcList.ActivePageIndex := PG_LIST;
      frmMain.SetFocus;
    end;
    if (Sender = sknbtnOptions) then
      TfrmOptions.Execute(Self);
    if (Sender = sknbtnDocuments) then
      OpenFolder(Config.GMBtnDocuments);
    if (Sender = sknbtnMusic) then
      OpenFolder(Config.GMBtnMusic);
    if (Sender = sknbtnPictures) then
      OpenFolder(Config.GMBtnPictures);
    if (Sender = sknbtnVideos) then
      OpenFolder(Config.GMBtnVideos);
    if (Sender = sknbtnExplore) then
      OpenFolder(Config.GMBtnExplore);
    if (Sender = sknbtnAbout) then
      TfrmAbout.Execute(Self);
  end;
end;

procedure TfrmGraphicMenu.pmWindowPopup(Sender: TObject);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := nil;
  if Assigned(vstList.GetFirstSelected) then
    NodeData := TVirtualTreeMethods.GetNodeItemData(vstList.GetFirstSelected, vstList);

  mniRun.Enabled := Assigned(NodeData) and not(NodeData.IsSeparatorItem);
  mniRunAs.Enabled         := Assigned(NodeData) and not(NodeData.IsSeparatorItem) and (NodeData.IsFileItem);
  mniRunAsAdmin.Enabled    := Assigned(NodeData) and not(NodeData.IsSeparatorItem) and (NodeData.IsFileItem);
  mniOpenFolderSw.Enabled  := Assigned(NodeData) and NodeData.IsFileItem;
  mniProperty.Enabled  := Assigned(NodeData);
end;

procedure TfrmGraphicMenu.PopulateListTree(const ATree: TVirtualStringTree);
begin
  ATree.Clear;
  ATree.BeginUpdate;
  try
    //Populate and get icons from first level
    ASuiteInstance.MainTree.IterateSubtree(nil, TVirtualTreeMethods.AddNodeInTreeFromMainTree, @ATree);
  finally
    ATree.EndUpdate;
    //Check nodes path
    TVirtualTreeMethods.CheckVisibleNodePathExe(ATree);
  end;
end;

procedure TfrmGraphicMenu.PopulateSpecialTree(const ATree: TVirtualStringTree;
  AList: TBaseItemsList; MaxItems: Integer);
var
  NewNodeData  : PTreeDataX;
  NewNode      : PVirtualNode;
  I, ItemCount : Integer;
begin
  ATree.Clear;
  ATree.BeginUpdate;
  try
    //Change node height and imagelist
    TVirtualTreeMethods.ChangeTreeIconSize(ATree, False);
    //Set limit based on MaxItems or AList.Count
    if MaxItems < AList.Count then
      ItemCount := MaxItems
    else
      ItemCount := AList.Count;
    for I := 0 to ItemCount - 1 do
    begin
      if Assigned(AList[I]) then
      begin
        //Create MenuItem
        if Assigned(AList[I]) then
        begin
          NewNode     := ATree.AddChild(nil);
          NewNodeData := ATree.GetNodeData(NewNode);
          //References
          NewNodeData.pNodeList := TvCustomRealNodeData(AList[I]).pNode;
        end
        else
          AList.Delete(I);
      end;
    end;
  finally
    ATree.EndUpdate;
    ATree.ValidateNode(ATree.RootNode, True);
    //Check nodes path
    TVirtualTreeMethods.CheckVisibleNodePathExe(ATree);
  end;
end;

procedure TfrmGraphicMenu.sknbtnEjectClick(Sender: TObject);
begin
  EjectDialog(Sender);
end;

procedure TfrmGraphicMenu.sknbtnExitClick(Sender: TObject);
begin
  frmMain.CloseASuite(False);
end;

procedure TfrmGraphicMenu.sknbtnListClick(Sender: TObject);
begin
  //Change node height and imagelist
  TVirtualTreeMethods.ChangeTreeIconSize(vstList, Config.GMSmallIconSize);
  PopulateListTree(vstList);
  edtSearch.Text := '';
end;

procedure TfrmGraphicMenu.sknbtnMFUClick(Sender: TObject);
begin
  PopulateSpecialTree(vstList, ASuiteManager.ListManager.MFUList, Config.MFUNumber);
end;

procedure TfrmGraphicMenu.sknbtnRecentsClick(Sender: TObject);
begin
  PopulateSpecialTree(vstList, ASuiteManager.ListManager.MRUList, Config.MRUNumber);
end;

procedure TfrmGraphicMenu.OpenMenu;
begin
  //Fade in now
  FOpening := True;
  tmrFader.Enabled:= True;
  //Show frmMenu
  Self.Show;
  Self.BringToFront;
  SetForegroundWindow(Self.Handle);
  if Not(frmMain.Visible) then
    frmMain.Hide;
end;

procedure TfrmGraphicMenu.tmrCheckItemsTimer(Sender: TObject);
begin
  if Config.ASuiteState = lsNormal then
    TVirtualTreeMethods.CheckVisibleNodePathExe(vstList);
end;

procedure TfrmGraphicMenu.tmrFaderTimer(Sender: TObject);
begin
  if FOpening then
  begin
    if (Self.AlphaBlendValue < 225) and Config.GMFade then
   	  Self.AlphaBlendValue := Self.AlphaBlendValue + 30
    else begin
      Self.AlphaBlendValue := 255;
      tmrFader.Enabled     := False;
    end;
  end
  else begin
    if (Self.AlphaBlendValue > 30) and Config.GMFade then
      Self.AlphaBlendValue := Self.AlphaBlendValue - 30
    else begin
      Self.AlphaBlendValue := 0;
      tmrFader.Enabled     := False;
      Self.Hide;
    end;
  end;
end;

end.
