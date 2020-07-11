{
Copyright (C) 2006-2020 Matteo Salvi

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
  LCLIntf, LCLType, Classes, Forms, StdCtrls, ExtCtrls, ComCtrls,
  Controls, Graphics, Dialogs, SysUtils, VirtualTrees, Menus, Windows,
  Lists.Base, BCImageTab, BCImageButton, DefaultTranslator, BGRASpeedButton;

type

  { TfrmGraphicMenu }

  TfrmGraphicMenu = class(TForm)
    edtSearch: TEdit;
    imgDriveSpace: TImage;
    imgDivider2: TImage;
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
    imgLogo: TImage;
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
    procedure edtSearchClick(Sender: TObject);
    procedure edtSearchEnter(Sender: TObject);
    procedure edtSearchMouseEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pmWindowClose(Sender: TObject);
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
    procedure btnSearchClick(Sender: TObject);
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
    procedure vstListHotChange(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode);
  private
    { Private declarations }
    FOpening : Boolean;
    FSearchButton: TBGRASpeedButton;

    procedure OpenFolder(FolderPath: string);
    procedure UpdateDriveStats;
    procedure CheckUserPicture;

    procedure PopulateListTree(const ATree: TVirtualStringTree);
    procedure PopulateSpecialTree(const ATree: TVirtualStringTree; AList: TBaseItemsList; MaxItems: Integer);
    procedure SavePositionForm;
    procedure WMWindowPosChanging(Var Msg : TWMWindowPosChanging); message WM_WindowPosChanging;
    procedure HandleEdge(var AEdge: Integer; ASnapToEdge: Integer; ASnapDistance: Integer = 0);
  public
    { Public declarations }
    procedure OpenMenu;
    procedure CloseMenu;
  end;

var
  frmGraphicMenu : TfrmGraphicMenu;
  PrevWndProc: WNDPROC;

implementation

{$R *.lfm}

uses
  Forms.Main, Utility.System, Kernel.Consts, AppConfig.Main, DataModules.Icons,
  Forms.About, NodeDataTypes.Base, Kernel.Enumerations, Forms.Options,
  Utility.Misc, VirtualTree.Events, VirtualTree.Methods, Kernel.Types,
  NodeDataTypes.Custom, GraphicMenu.ThemeEngine, Kernel.ResourceStrings;

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
const
  Margin = 5;
var
  Rect: TRect;
  MouseX, MouseY: LongInt;
begin
  if uMsg = WM_NCHITTEST then
  begin
    Result := Windows.DefWindowProc(Ahwnd, uMsg, wParam, lParam);
    MouseX := GET_X_LPARAM(lParam);
    MouseY := GET_Y_LPARAM(lParam);
    with Rect do
    begin
      Left := MouseX - frmGraphicMenu.BoundsRect.Left;
      Right := frmGraphicMenu.BoundsRect.Right - MouseX;
      Top := MouseY - frmGraphicMenu.BoundsRect.Top;
      Bottom := frmGraphicMenu.BoundsRect.Bottom - MouseY;
      if (Top < Margin) and (Left < Margin) then
        Result := windows.HTTOPLEFT
      else if (Top < Margin) and (Right < Margin) then
        Result := windows.HTTOPRIGHT
      else if (Bottom < Margin) and (Left < Margin) then
        Result := windows.HTBOTTOMLEFT
      else if (Bottom < Margin) and (Right < Margin) then
        Result := windows.HTBOTTOMRIGHT
      else if (Top < Margin) then
        Result := windows.HTTOP
      else if (Left < Margin) then
        Result := windows.HTLEFT
      else if (Bottom < Margin) then
        Result := windows.HTBOTTOM
      else if (Right < Margin) then
        Result := windows.HTRIGHT;
    end;
    Exit;
  end;
  Result := CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
end;

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
      if Assigned(FSearchButton) then
        FSearchButton.ImageIndex := TThemeEngine.Create.CancelIcon;

      //Do search
      //Change node height and imagelist
      TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, False);
      frmMain.DoSearchItem(vstList, edtSearch.Text, stName);
      vstList.SortTree(-1, sdAscending);

      //Select node
      Node := vstList.GetFirst;
      if Assigned(Node) then
        vstList.Selected[Node] := True;
    end
    else begin
      if Assigned(FSearchButton) then
        FSearchButton.ImageIndex := TThemeEngine.Create.SearchIcon;

      //Change node height and imagelist
      TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, Config.GMSmallIconSize);
      PopulateListTree(vstList);
    end;
  finally
    vstList.EndUpdate;
  end;

  //We must repaint SearchButton because TEdit
  if Assigned(FSearchButton) then
    FSearchButton.Invalidate;
end;

procedure TfrmGraphicMenu.btnSearchClick(Sender: TObject);
begin
  edtSearch.Text := '';
end;

procedure TfrmGraphicMenu.CloseMenu;
begin
  //Fade in out
  FOpening := False;
  tmrFader.Enabled:= True;
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

procedure TfrmGraphicMenu.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
begin
  if (Parent = nil) and
    ((Msg.WindowPos.X <> 0) or (Msg.WindowPos.Y <> 0)) and
    ((Msg.WindowPos.cx = Self.Width) and (Msg.WindowPos.cy = Self.Height)) then
  begin
    HandleEdge(Msg.WindowPos.x, Monitor.WorkareaRect.Left, 0);
    HandleEdge(Msg.WindowPos.y, Monitor.WorkareaRect.Top, 0);
    HandleEdge(Msg.WindowPos.x, Monitor.WorkareaRect.Right, Self.Width);
    HandleEdge(Msg.WindowPos.y, Monitor.WorkareaRect.Bottom, Self.Height);
  end;

  inherited;
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
    sTempPath := Config.Paths.SuitePathCurrentTheme + Config.GMPersonalPicture
  else begin
    sTempPath := Config.Paths.RelativeToAbsolute(Config.GMPersonalPicture);
    if Not FileExists(sTempPath) then
      sTempPath := Config.Paths.SuitePathCurrentTheme + 'PersonalPicture.png';
  end;
  imgPersonalPicture.Picture.LoadFromFile(sTempPath);
  imgPersonalPicture.Visible := (FileExists(sTempPath));
  imgUserFrame.Visible := (FileExists(sTempPath));
end;

procedure TfrmGraphicMenu.UpdateDriveStats;
var
  dblDriveSize: Double;
  Drive: Char;
  dblDriveUsed: Double;
begin
  //Calculate and display the drive size
  Drive := Config.Paths.SuiteDrive[1];
  dblDriveSize := DiskSize(Ord(Drive) - 64);
  dblDriveUsed := dblDriveSize - DiskFree(Ord(Drive) - 64);
  imgDriveSpace.Width := Round(dblDriveUsed / dblDriveSize * (imgDriveBackground.Width - 4));
  lblDriveSpace.Caption := Format(msgGMHardDiskSpace, [DiskFreeString(Drive, True), DiskSizeString(Drive, True)]);
end;

procedure TfrmGraphicMenu.OpenFolder(FolderPath: string);
var
  ErrorCode: Boolean;
  sPath: string;
begin
  sPath := Config.Paths.RelativeToAbsolute(FolderPath);
  ErrorCode := OpenDocument(PChar(sPath));
  if ErrorCode then
    ShowMessageFmtEx(msgErrGeneric, ['', SysErrorMessage(GetLastError)], True);
end;

procedure TfrmGraphicMenu.FormCreate(Sender: TObject);
begin
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback)));

  TVirtualTreeEvents.Create.SetupVSTGraphicMenu(vstList, Self);

  //Load graphics
  TThemeEngine.Create.SetupThemeEngine(Self);
  TThemeEngine.Create.LoadTheme;

  //Set PopUpMenu's ImageIndexes
  pmWindow.Images := dmImages.ilSmallIcons;
  mniRun.ImageIndex := Config.IconsManager.GetIconIndex('run');
  mniProperty.ImageIndex := Config.IconsManager.GetIconIndex('property');

  //Position
  if Config.GMPositionTop <> -1 then
    Self.Top  := Config.GMPositionTop
  else
    Self.Top  := Screen.WorkAreaRect.Bottom - Height;
  if Config.GMPositionLeft <> -1 then
    Self.Left  := Config.GMPositionLeft
  else
    Self.Left  := Screen.WorkAreaRect.Right - Width;

  //Create speedbutton for Search
  //TODO: Make a separate LCL component (TEdit + TBGRASpeedButton)
  FSearchButton := TBGRASpeedButton.Create(Self);
  FSearchButton.Flat   := True;
  FSearchButton.Parent := edtSearch;
  FSearchButton.Width  := 20;
  FSearchButton.Align  := alRight;
  FSearchButton.Constraints.MaxHeight := 18;
  FSearchButton.Images := dmImages.ilSmallIcons;
  FSearchButton.ImageIndex := TThemeEngine.Create.SearchIcon;
  FSearchButton.Cursor := crArrow;
  FSearchButton.OnClick := btnSearchClick;

  //Set margin in edtSearch (so text doesn't go behind the button)
  SendMessage(edtSearch.Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, MakeLParam(0, FSearchButton.Width))
end;

procedure TfrmGraphicMenu.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearchButton);
end;

procedure TfrmGraphicMenu.edtSearchClick(Sender: TObject);
begin
end;

procedure TfrmGraphicMenu.edtSearchEnter(Sender: TObject);
begin
  //We must repaint SearchButton because TEdit
  if Assigned(FSearchButton) then
    FSearchButton.Invalidate;
end;

procedure TfrmGraphicMenu.edtSearchMouseEnter(Sender: TObject);
begin
  //We must repaint SearchButton because TEdit
  if Assigned(FSearchButton) then
    FSearchButton.Invalidate;
end;

procedure TfrmGraphicMenu.pmWindowClose(Sender: TObject);
begin
  vstList.ClearSelection;
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
  CurrentNode := vstList.GetFirstSelected();
  case Ord(Key) of
    VK_UP:
      begin
        Key := 0;
        CurrentNode := vstList.GetPreviousVisible(vstList.GetFirstSelected());
        if Not Assigned(CurrentNode) then
          CurrentNode := vstList.GetLast;
      end;
    VK_DOWN:
      begin
        Key := 0;
        CurrentNode := vstList.GetNextVisible(vstList.GetFirstSelected());
        if Not Assigned(CurrentNode) then
          CurrentNode := vstList.GetFirst;
      end;
    VK_RETURN:
      begin
        Key := 0;
        if Assigned(CurrentNode) then
        begin
          NodeData := TVirtualTreeMethods.Create.GetNodeItemData(CurrentNode, vstList);
          if Assigned(NodeData) then
          begin
            case NodeData.DataType of
              vtdtCategory:
                begin
                  if (ssCtrl in Shift) then
                    TVirtualTreeMethods.Create.ExecuteNode(vstList, CurrentNode, rmNormal, False)
                  else
                    vstList.Expanded[CurrentNode] := Not(vstList.Expanded[CurrentNode]);
                end;
              vtdtFile: TVirtualTreeMethods.Create.ExecuteNode(vstList, CurrentNode, rmNormal, False);
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
  end;

  if Assigned(CurrentNode) then
    vstList.Selected[CurrentNode] := True;
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
  sknbtnList.Pressed := True;
  //Clear edtSearch and focus it
  edtSearch.Text := '';
  Self.FocusControl(edtSearch);
  //Change node height and imagelist
  TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, Config.GMSmallIconSize);
  //Clear and populate virtualtree
  PopulateListTree(vstList);
  UpdateDriveStats;
  //Enable or disable tabs
  sknbtnRecents.Visible := Config.MRU;
  sknbtnMFU.Visible := Config.MFU;
  //Timer
  tmrCheckItems.Enabled := True;
end;

procedure TfrmGraphicMenu.imgLogoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(frmGraphicMenu.Handle, WM_SYSCOMMAND, SC_DRAGMOVE, 0);

    SavePositionForm;
  end;
end;

procedure TfrmGraphicMenu.imgPersonalPictureClick(Sender: TObject);
var
  TempString : string;
begin
  TempString := '';
  OpenDialog1.Filter     := msgFilterPicture;
  OpenDialog1.InitialDir := ExtractFileDir(Config.Paths.RelativeToAbsolute(Config.GMPersonalPicture));
  if OpenDialog1.Execute then
  begin
    TempString := OpenDialog1.FileName;
    imgPersonalPicture.Picture.LoadFromFile(TempString);
    Config.GMPersonalPicture := Config.Paths.AbsoluteToRelative(TempString);
    Config.Changed := True;
  end;
end;

procedure TfrmGraphicMenu.mniPropertyClick(Sender: TObject);
begin
  TVirtualTreeMethods.Create.ShowItemProperty(Self, vstList, vstList.GetFirstSelected);
end;

procedure TfrmGraphicMenu.mniRunClick(Sender: TObject);
begin
  TVirtualTreeMethods.Create.ExecuteSelectedNodes(vstList, TRunMode(TMenuItem(Sender).Tag), False);
end;

procedure TfrmGraphicMenu.vstListHotChange(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode);
begin
  if Assigned(vstList.GetFirstSelected()) then
    vstList.ClearSelection;
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
    begin
      if not IsFormOpen('frmAbout') then
        Application.CreateForm(TfrmAbout, frmAbout);
      frmAbout.Show;
      frmAbout.SetFocus;
    end;
  end;
end;

procedure TfrmGraphicMenu.pmWindowPopup(Sender: TObject);
var
  NodeData : TvBaseNodeData;
begin
  if Assigned(vstList.GetFirstSelected()) then
  begin
    NodeData := TVirtualTreeMethods.Create.GetNodeItemData(vstList.GetFirstSelected, vstList);
    mniRun.Enabled := (NodeData.DataType <> vtdtSeparator);
    mniRunAs.Enabled         := (NodeData.DataType <> vtdtSeparator);
    mniRunAsAdmin.Enabled    := (NodeData.DataType <> vtdtSeparator);
    mniOpenFolderSw.Enabled  := (NodeData.DataType in [vtdtFile, vtdtFolder]);
  end;
end;

procedure TfrmGraphicMenu.PopulateListTree(const ATree: TVirtualStringTree);
begin
  ATree.Clear;
  ATree.BeginUpdate;
  try
    //Populate and get icons from first level
    Config.MainTree.IterateSubtree(nil, TVirtualTreeMethods.Create.AddNodeInTreeFromMainTree, @ATree);
  finally
    ATree.EndUpdate;
    //Check nodes path
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(ATree);
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
    TVirtualTreeMethods.Create.ChangeTreeIconSize(ATree, False);
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
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(ATree);
  end;
end;

procedure TfrmGraphicMenu.sknbtnEjectClick(Sender: TObject);
begin
  EjectDialog(Sender);
end;

procedure TfrmGraphicMenu.sknbtnExitClick(Sender: TObject);
begin
  frmMain.miExitClick(Sender);
end;

procedure TfrmGraphicMenu.sknbtnListClick(Sender: TObject);
begin
  //Change node height and imagelist
  TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, Config.GMSmallIconSize);
  PopulateListTree(vstList);
  edtSearch.Text := '';
end;

procedure TfrmGraphicMenu.sknbtnMFUClick(Sender: TObject);
begin
  PopulateSpecialTree(vstList, Config.ListManager.MFUList, Config.MFUNumber);
end;

procedure TfrmGraphicMenu.sknbtnRecentsClick(Sender: TObject);
begin
  PopulateSpecialTree(vstList, Config.ListManager.MRUList, Config.MRUNumber);
end;

procedure TfrmGraphicMenu.OpenMenu;
begin
  //Fade in now
  FOpening := True;
  tmrFader.Enabled:= True;
  //Show frmMenu
  Self.Show;
  SetForegroundWindow(Self.Handle);
  if Not(IsWindowVisible(frmMain.Handle)) then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmGraphicMenu.tmrCheckItemsTimer(Sender: TObject);
begin
  if Config.ASuiteState = lsNormal then
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(vstList);
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
