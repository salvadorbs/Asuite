unit ulXMLUtils;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, VirtualTrees, AppConfig, GTForm, System.UITypes,
  XMLIntf, msxmldom, XMLDoc, ulEnumerations, ulDatabase, DateUtils, xmldom;

{ Load list or settings from xml file }
type
  TImportListToTree = function(Tree: TVirtualStringTree;Node: IXMLNode;Parent: PVirtualNode): PVirtualNode of object;

  TImportOldListProcs = class //Class for IterateSubtree
    function ASuite1NodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
                               Parent: PVirtualNode): PVirtualNode;
    function wppLauncherNodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
                                    Parent: PVirtualNode): PVirtualNode;
  end;

procedure XMLToTree(Tree: TVirtualStringTree;CallBack: TImportListToTree;
                    XMLDoc: TXMLDocument);
procedure LoadXMLSettings(XMLDoc: TXMLDocument);

{ Methods to get values }
function GetStrPropertyXML(Node : IXMLNode;Name: String;Default: String): String;
function GetIntPropertyXML(Node : IXMLNode;Name: String;Default: Integer): Integer;
function GetBoolPropertyXML(Node : IXMLNode;Name: String;Default: Boolean): Boolean;

implementation

uses
  Main, ulNodeDataTypes, ulCommonUtils, ulTreeView, udImages, ulAppConfig,
  ulFileFolder, udClassicMenu, ulStringUtils;

function GetStrPropertyXML(Node : IXMLNode;Name: String;Default: String): String;
var
  PropertyNode: IXMLNode;
begin
  Result := Default;
  PropertyNode := Node.ChildNodes[Name];
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.Text <> '' then
      Result := PropertyNode.Text;
end;

function GetIntPropertyXML(Node : IXMLNode;Name: String;Default: Integer): Integer;
var
  PropertyNode: IXMLNode;
begin
  Result := Default;
  PropertyNode := Node.ChildNodes[Name];
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.Text <> '' then
      Result := StrToInt(PropertyNode.Text);
end;

function GetBoolPropertyXML(Node : IXMLNode;Name: String;Default: Boolean): Boolean;
var
  PropertyNode: IXMLNode;
begin
  Result := Default;
  PropertyNode := Node.ChildNodes[Name];
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.Text <> '' then
      Result := ulStringUtils.StrToBool(PropertyNode.Text);
end;

function TImportOldListProcs.ASuite1NodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
                                          Parent: PVirtualNode): PVirtualNode;
var
  NodeData : PBaseData;
  schDate, schTime   : string;
  CustomRealNodeData : TvCustomRealNodeData;
begin
  Result := nil;
  if ((XMLNode.HasAttribute('name')) or (XMLNode.NodeName = 'Separator')) and
     ((XMLNode.NodeName = 'Software') or (XMLNode.NodeName = 'Category') or
      (XMLNode.NodeName = 'Separator')) then
  begin
    //Create a new XMLNode
    //Get item type
    if (XMLNode.NodeName = 'Category') then
      Result := Tree.AddChild(Parent, CreateNodeData(vtdtCategory))
    else
      if (XMLNode.NodeName = 'Software') then
        Result := Tree.AddChild(Parent, CreateNodeData(vtdtFile))
      else
        if XMLNode.NodeName = 'Separator' then
          Result := Tree.AddChild(Parent, CreateNodeData(vtdtSeparator));
    //Add checkbox
    if Config.ASuiteState = asImporting then
      Tree.CheckType[Result] := ctTriStateCheckBox;
    NodeData := Tree.GetNodeData(Result);
    NodeData.Data.pNode  := Result;
    CustomRealNodeData   := TvCustomRealNodeData(NodeData.Data);
    //Get base properties
    if (CustomRealNodeData.DataType <> vtdtSeparator) then
    begin
      CustomRealNodeData.Name     := String(XMLNode.Attributes['name']);
      CustomRealNodeData.PathIcon := GetStrPropertyXML(XMLNode, 'PathIcon', '');
      CustomRealNodeData.HideFromMenu := GetBoolPropertyXML(XMLNode, 'HideSoftwareMenu', false);
      CustomRealNodeData.SchMode  := TSchedulerMode(GetIntPropertyXML (XMLNode, 'SchedulerMode',0));
      //Scheduler date and time
      schDate := GetStrPropertyXML(XMLNode, 'SchedulerDate', '');
      schTime := GetStrPropertyXML(XMLNode, 'SchedulerTime', '');
      if (schDate <> '') and (schTime <> '') then
        CustomRealNodeData.SchDateTime := StrToDateTime(schDate + ' ' + schTime);
      CustomRealNodeData.ActionOnExe := TActionOnExecute(GetIntPropertyXML(XMLNode, 'ActionOnExe',0));
      CustomRealNodeData.Autorun     := TAutorunType(GetIntPropertyXML(XMLNode, 'Autorun',0));
      CustomRealNodeData.AutorunPos  := GetIntPropertyXML (XMLNode, 'AutorunPosition',0);
      CustomRealNodeData.WindowState := GetIntPropertyXML (XMLNode, 'WindowState',0);
      CustomRealNodeData.Hotkey      := GetBoolPropertyXML(XMLNode, 'HotKey',False);
      CustomRealNodeData.HotkeyMod   := GetIntPropertyXML (XMLNode, 'HotKeyModifier',0);
      CustomRealNodeData.HotkeyCode  := GetIntPropertyXML (XMLNode, 'HotKeyCode',0);
      //Check if it is a software, so get software properties
      if (CustomRealNodeData.DataType = vtdtFile) then
      begin
        with TvFileNodeData(CustomRealNodeData) do
        begin
          NoMRU       := GetBoolPropertyXML(XMLNode, 'DontInsertMRU',false);
          PathExe     := GetStrPropertyXML (XMLNode, 'PathExe', '');
          Parameters  := GetStrPropertyXML (XMLNode, 'Parameters', '');
          WorkingDir  := GetStrPropertyXML (XMLNode, 'WorkingDir', '');
          ShortcutDesktop := GetBoolPropertyXML(XMLNode, 'ShortcutDesktop',false);
        end;
      end;
    end;
  end;
end;

function TImportOldListProcs.wppLauncherNodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
                                               Parent: PVirtualNode): PVirtualNode;
var
  NodeData     : PBaseData;
  CustomRealNodeData : TvCustomRealNodeData;
begin
  Result := nil;
  if ((XMLNode.HasAttribute('name')) or (XMLNode.NodeName = 'separator')) and
     ((XMLNode.NodeName = 'file') or (XMLNode.NodeName = 'files') or
      (XMLNode.NodeName = 'separator')) then
  begin
    //Create a new XMLNode
    //Get item type
    if (XMLNode.NodeName = 'files') then
      Result := tree.AddChild(Parent, CreateNodeData(vtdtCategory))
    else
      if (XMLNode.NodeName = 'file') then
        Result := tree.AddChild(Parent, CreateNodeData(vtdtFile))
      else
        if XMLNode.NodeName = 'separator' then
          Result := Tree.AddChild(Parent, CreateNodeData(vtdtSeparator));
    //Add checkbox
    Tree.CheckType[Result] := ctTriStateCheckBox;
    NodeData := Tree.GetNodeData(Result);
    NodeData.Data.pNode := Result;
    CustomRealNodeData  := TvCustomRealNodeData(NodeData.Data);
    //Get base properties
    if CustomRealNodeData.DataType <> vtdtSeparator then
    begin
      CustomRealNodeData.Name     := String(XMLNode.Attributes['name']);
      CustomRealNodeData.PathIcon := GetStrPropertyXML(XMLNode,'icon','');
      CustomRealNodeData.HideFromMenu := GetBoolPropertyXML(XMLNode, 'HideSoftwareMenu', false);
      //Check if it is a software, so get software properties
      if (CustomRealNodeData.DataType = vtdtFile) then
      begin
        with TvFileNodeData(CustomRealNodeData) do
        begin
          NoMRU       := GetBoolPropertyXML(XMLNode, 'DontInsertMRU',false);
          PathExe     := GetStrPropertyXML (XMLNode, 'path','');
          Parameters  := GetStrPropertyXML (XMLNode, 'parameters','');
          WorkingDir  := GetStrPropertyXML (XMLNode, 'WorkingDir','');
          WindowState := GetIntPropertyXML (XMLNode, 'WindowState',0);
          ShortcutDesktop := GetBoolPropertyXML(XMLNode, 'ShortcutDesktop',false);
          ActionOnExe := TActionOnExecute(GetIntPropertyXML(XMLNode, 'ActionOnExe',0));
          Autorun     := TAutorunType(GetIntPropertyXML(XMLNode, 'Autorun',0));
          AutorunPos  := GetIntPropertyXML(XMLNode, 'AutorunPosition',0);
        end;
      end;
    end;
  end;
end;

procedure XMLToTree(Tree: TVirtualStringTree;CallBack: TImportListToTree;
  XMLDoc: TXMLDocument);
var
  cXMLNode : IXMLNode;

  procedure ProcessNode(XMLNode : IXMLNode;TreeNode : PVirtualNode);
  var
    cNode : IXMLNode;
  begin
    if XMLNode = nil then Exit;
    //Import xml node in vstListImp
    TreeNode := CallBack(Tree, XMLNode, TreeNode);
    //Next nodes
    cNode := XMLNode.ChildNodes.First;
    while Assigned(cNode) do
    begin
      ProcessNode(cNode,TreeNode);
      cNode := cNode.NextSibling;
    end;
  end;

begin
  Tree.Clear;
  Tree.BeginUpdate;
  cXMLNode := XMLDoc.DocumentElement.ChildNodes.First;
  while Assigned(cXMLNode) do
  begin
    ProcessNode(cXMLNode,nil);
    cXMLNode := cXMLNode.NextSibling;
  end;
  Tree.EndUpdate;
end;

procedure LoadXMLSettings(XMLDoc: TXMLDocument);
var
  Node, tvFontStyle : IXMLNode;
  I      : Integer;
begin
  if XMLDoc.Active then
  begin
    //ASuite 1.x - wppLauncher
    Node := XMLDoc.DocumentElement.ChildNodes['Option'];
    //Get GMTheme before everything (so ASuite know where icons folder)
    Config.GMTheme        := GetStrPropertyXML(Node, 'MenuTheme','Default');
    //General
    Config.StartWithWindows   := GetBoolPropertyXML(Node, 'StartOnWindowsStartup', false);
    Config.ShowPanelAtStartUp := GetBoolPropertyXML(Node, 'StartUpShowPanel', true);
    Config.ShowMenuAtStartUp  := GetBoolPropertyXML(Node, 'StartUpShowMenu', false);
    //Main Form
    Config.CustomTitleString := GetStrPropertyXML(Node, 'CustomTitleString', APP_TITLE);
    Config.UseCustomTitle    := GetBoolPropertyXML(Node, 'CustomTitle', false);
    Config.HideTabSearch     := GetBoolPropertyXML(Node, 'HideSearch', false);
    //Main Form - Position and size
    Config.HoldSize    := GetBoolPropertyXML(Node, 'HoldSize', false);
    Config.AlwaysOnTop := GetBoolPropertyXML(Node, 'MainOnTop', false);
    //frmMain's size
    frmMain.Width      := GetIntPropertyXML(Node,'ListFormWidth',frmMainWidth);
    frmMain.Height     := GetIntPropertyXML(Node,'ListFormHeight',frmMainHeight);
    //Hotkey
    Config.HotKey         := GetBoolPropertyXML(Node, 'ActiveHotKey',true);
    //Window Hotkey
    Config.WindowHotkeyMod  := GetIntPropertyXML(Node,'HotKeyModifier',-1);
    Config.WindowHotkeyCode := GetIntPropertyXML(Node,'HotKeyCode',-1);
    Config.WindowHotkey     := GetBoolPropertyXML(Node,'HotKey',false);
    //Menu Hotkey
    Config.MenuHotkeyMod  := GetIntPropertyXML(Node,'MenuHotKeyModifier',-1);
    Config.MenuHotkeyCode := GetIntPropertyXML(Node,'MenuHotKeyCode',-1);
    Config.MenuHotkey     := GetBoolPropertyXML(Node,'MenuHotKey',false);
    //frmMain position
    SetFormPosition(frmMain, GetIntPropertyXML(Node,'ListFormLeft',frmMain.Left),
                             GetIntPropertyXML(Node,'ListFormTop',frmMain.Top));
    frmMain.Position := poDesigned;
    //Main Form - Treevew
    Config.TVBackgroundPath := GetStrPropertyXML(Node, 'BackgroundPath','');
    Config.TVBackground     := GetBoolPropertyXML(Node, 'Background',False);
    Config.TVAutoOpClCats   := GetBoolPropertyXML(Node, 'AutoOpClCategories',False);
    //Treeview Font
    Config.TVFont.Name      := GetStrPropertyXML(Node, 'TreeViewFontName','MS Sans Serif');
    tvFontStyle             := Node.ChildNodes['TreeViewFontStyle'];
    if Assigned(tvFontStyle) then
    begin
      if GetBoolPropertyXML(tvFontStyle,'fsBold',false) then
        Config.TVFont.Style := Config.TVFont.Style + [fsBold];
      if GetBoolPropertyXML(tvFontStyle,'fsItalic',false) then
        Config.TVFont.Style := Config.TVFont.Style + [fsItalic];
      if GetBoolPropertyXML(tvFontStyle,'fsUnderline',false) then
        Config.TVFont.Style := Config.TVFont.Style + [fsUnderline];
      if GetBoolPropertyXML(tvFontStyle,'fsStrikeOut',false) then
        Config.TVFont.Style := Config.TVFont.Style + [fsStrikeOut];
    end;
    Config.TVFont.Size    := GetIntPropertyXML(Node,'TreeViewFontSize',8);
    Config.TVFont.Color   := GetIntPropertyXML(Node,'TreeViewFontColor',clWindowText);
    //MRU
    Config.MRU            := GetBoolPropertyXML(Node, 'ActiveMRU',true);
    Config.SubMenuMRU     := GetBoolPropertyXML(Node, 'ActiveSubMenuMRU',false);
    Config.MRUNumber      := GetIntPropertyXML(Node, 'MRUNumber',5);
    //Backup
    Config.Backup         := GetBoolPropertyXML(Node, 'ActiveBackup',true);
    Config.BackupNumber   := GetIntPropertyXML(Node, 'BackupNumber',5);
    //Other functions
    Config.Autorun        := GetBoolPropertyXML(Node, 'ActiveAutorun',true);
    Config.Cache          := GetBoolPropertyXML(Node, 'ActiveCache',true);
    Config.Scheduler      := GetBoolPropertyXML(Node, 'ActiveScheduler',true);
    //Execution
    Config.ActionOnExe    := TActionOnExecute(GetIntPropertyXML(Node, 'ActionOnExe',0));
    Config.RunSingleClick := GetBoolPropertyXML(Node, 'RunSingleClick',false);
    //Trayicon
    Config.TrayCustomIconPath := GetStrPropertyXML(Node, 'TrayIconPath','');
    Config.TrayIcon           := GetBoolPropertyXML(Node, 'ActiveTrayIcon',true);
    Config.ActionClickLeft    := GetIntPropertyXML(Node, 'ActionClickLeft',0);
    Config.ActionClickRight   := GetIntPropertyXML(Node, 'ActionClickRight',2);
    Config.UseCustomTitle     := GetBoolPropertyXML(Node, 'ClassicMenu',false);
    //Only default menu
    Config.GMFade         := GetBoolPropertyXML(Node, 'MenuFade',true);
    Config.GMPersonalPicture := GetStrPropertyXML(Node, 'MenuPersonalPicture','Default');
    //Mouse sensors
    for I := 0 to 3 do
    begin
      Config.SensorLeftClick[I]  := GetIntPropertyXML(Node.ChildNodes['Mouse'],'SensorLeftClick' + IntToStr(I), 0);
      Config.SensorRightClick[I] := GetIntPropertyXML(Node.ChildNodes['Mouse'],'SensorRightClick' + IntToStr(I), 0);
    end;
  end;
end;

end.
