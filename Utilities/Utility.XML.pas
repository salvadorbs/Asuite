unit Utility.XML;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, VirtualTrees, Kernel.Consts, System.UITypes,
  XMLIntf, msxmldom, XMLDoc, Kernel.Enumerations, DateUtils, xmldom;

{ Load list or settings from xml file }
type
  TImportListToTree = function(Tree: TVirtualStringTree;Node: IXMLNode;Parent: PVirtualNode): PVirtualNode of object;

  TImportOldListProcs = class
    class function ASuite1NodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
                                 Parent: PVirtualNode): PVirtualNode;
    class function wppLauncherNodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
                                     Parent: PVirtualNode): PVirtualNode;
    class function PStartNodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
                                Parent: PVirtualNode): PVirtualNode;
  end;

procedure XMLToTree(Tree: TVirtualStringTree;ListType: TListType;
                    XMLDoc: TXMLDocument);
procedure LoadDatabaseFromXML(FileName: string);
procedure LoadXMLSettings(XMLDoc: TXMLDocument);
function XMLToShortcut(Node: IXMLNode; AFieldCode, AFieldMod: string): TShortcut;
Function GetHotKeyCode(KeyCode: Integer): Integer;
Function GetHotKeyMod(KeyMod: Integer): TShiftState;

{ Methods to get values }
function GetStrPropertyXML(Node: IXMLNode; Name: String; Default: String): String;
function GetIntPropertyXML(Node: IXMLNode; Name: String; Default: Integer): Integer;
function GetBoolPropertyXML(Node: IXMLNode; Name: String; Default: Boolean): Boolean;

implementation

uses
  Forms.Main, NodeDataTypes.Custom, Utility.Misc, AppConfig.Main, Kernel.Types,
  Utility.Conversions, VirtualTree.Methods, NodeDataTypes.Files, Menus,
  Kernel.Logger;

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
      Result := Utility.Conversions.StrToBool(PropertyNode.Text);
end;

class function TImportOldListProcs.ASuite1NodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
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
      Result := Tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtCategory))
    else
      if (XMLNode.NodeName = 'Software') then
        Result := Tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtFile))
      else
        if XMLNode.NodeName = 'Separator' then
          Result := Tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtSeparator));
    //Add checkbox
    if Config.ASuiteState = lsImporting then
      Tree.CheckType[Result] := ctTriStateCheckBox;
    NodeData := Tree.GetNodeData(Result);
    NodeData.Data.SetPointerNode(Result);
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
      CustomRealNodeData.Hotkey      := XMLToShortcut(XMLNode, 'HotKeyCode', 'HotKeyModifier');
      //Check if it is a software, so get software properties
      if (CustomRealNodeData.DataType = vtdtFile) then
      begin
        with TvFileNodeData(CustomRealNodeData) do
        begin
          NoMRU       := GetBoolPropertyXML(XMLNode, 'DontInsertMRU',false);
          PathFile    := GetStrPropertyXML (XMLNode, 'PathExe', '');
          Parameters  := GetStrPropertyXML (XMLNode, 'Parameters', '');
          WorkingDir  := GetStrPropertyXML (XMLNode, 'WorkingDir', '');
          ShortcutDesktop := GetBoolPropertyXML(XMLNode, 'ShortcutDesktop',false);
        end;
      end;
    end;
  end;
end;

function XMLToShortcut(Node: IXMLNode; AFieldCode, AFieldMod: string): TShortcut;
var
  Key: Word;
  ShiftState: TShiftState;
begin
  Result := 0;
  Key := GetHotKeyCode(GetIntPropertyXML(Node, AFieldCode, -1));
  ShiftState := GetHotKeyMod(GetIntPropertyXML(Node, AFieldMod, -1));
  if (Key <> 0) and (ShiftState <> []) then
    Result := ShortCut(Key, ShiftState);
end;

class function TImportOldListProcs.PStartNodeToTree(Tree: TVirtualStringTree;
  XMLNode: IXMLNode; Parent: PVirtualNode): PVirtualNode;
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
      Result := tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtCategory))
    else
      if (XMLNode.NodeName = 'file') then
        Result := tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtFile))
      else
        if XMLNode.NodeName = 'separator' then
          Result := Tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtSeparator));
    //Add checkbox
    Tree.CheckType[Result] := ctTriStateCheckBox;
    NodeData := Tree.GetNodeData(Result);
    NodeData.Data.SetPointerNode(Result);
    CustomRealNodeData  := TvCustomRealNodeData(NodeData.Data);
    //Get base properties
    if CustomRealNodeData.DataType <> vtdtSeparator then
    begin
      CustomRealNodeData.Name     := String(XMLNode.Attributes['name']);
      CustomRealNodeData.PathIcon := GetStrPropertyXML(XMLNode,'icon','');
      //Check if it is a software, so get software properties
      if (CustomRealNodeData.DataType = vtdtFile) then
      begin
        with TvFileNodeData(CustomRealNodeData) do
        begin
          NoMRU       := GetBoolPropertyXML(XMLNode, 'DontInsertMRU',false);
          PathFile    := GetStrPropertyXML (XMLNode, 'path','');
          Parameters  := GetStrPropertyXML (XMLNode, 'parameters','');
          WorkingDir  := GetStrPropertyXML (XMLNode, 'directory','');
          WindowState := GetIntPropertyXML (XMLNode, 'windowstate',0);
          Autorun     := TAutorunType(GetIntPropertyXML(XMLNode, 'onstartup',0));
          if Autorun = atNever then
            Autorun := TAutorunType(GetIntPropertyXML(XMLNode, 'onexit',0));
        end;
      end;
    end;
  end;
end;

class function TImportOldListProcs.wppLauncherNodeToTree(Tree: TVirtualStringTree;XMLNode: IXMLNode;
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
      Result := tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtCategory))
    else
      if (XMLNode.NodeName = 'file') then
        Result := tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtFile))
      else
        if XMLNode.NodeName = 'separator' then
          Result := Tree.AddChild(Parent, TVirtualTreeMethods.Create.CreateNodeData(vtdtSeparator));
    //Add checkbox
    Tree.CheckType[Result] := ctTriStateCheckBox;
    NodeData := Tree.GetNodeData(Result);
    NodeData.Data.SetPointerNode(Result);
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
          PathFile    := GetStrPropertyXML (XMLNode, 'path','');
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

procedure XMLToTree(Tree: TVirtualStringTree; ListType: TListType;
  XMLDoc: TXMLDocument);
var
  cXMLNode : IXMLNode;
  AConvMethod: TImportListToTree;

  procedure ProcessNode(XMLNode : IXMLNode;TreeNode : PVirtualNode);
  var
    cNode : IXMLNode;
  begin
    if XMLNode = nil then Exit;

    //Import xml node in vstListImp
    TreeNode := AConvMethod(Tree, XMLNode, TreeNode);

    //Next nodes
    cNode := XMLNode.ChildNodes.First;
    while Assigned(cNode) do
    begin
      ProcessNode(cNode,TreeNode);
      cNode := cNode.NextSibling;
    end;
  end;

begin
  case ListType of
    ltASuite1:
    begin
      TASuiteLogger.Info('Found ASuite 1.x List (%s)', [XMLDoc.FileName]);
      AConvMethod := TImportOldListProcs.ASuite1NodeToTree;
    end;
    ltwppLauncher1:
    begin
      TASuiteLogger.Info('Found winPenPack Launcher 1.x List (%s)', [XMLDoc.FileName]);
      AConvMethod := TImportOldListProcs.wppLauncherNodeToTree;
    end;
    ltPStart1:
    begin
      TASuiteLogger.Info('Found PStart 1.x List (%s)', [XMLDoc.FileName]);
      AConvMethod := TImportOldListProcs.PStartNodeToTree;
    end;
  end;

  Tree.Clear;
  Tree.BeginUpdate;
  try
    cXMLNode := XMLDoc.DocumentElement.ChildNodes.First;

    while Assigned(cXMLNode) do
    begin
      ProcessNode(cXMLNode,nil);
      cXMLNode := cXMLNode.NextSibling;
    end;
  finally
    Tree.EndUpdate;
  end;
end;

procedure LoadDatabaseFromXML(FileName: string);
var
  XMLDoc: TXMLDocument;
begin
  TASuiteLogger.Info('Load XML List', []);
  //Create XMLDoc
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.FileName := FileName;
    XMLDoc.Active := True;
    //Load list and settings
    if (XMLDoc.DocumentElement.NodeName = 'ASuite') then
    begin
      LoadXMLSettings(XMLDoc);
      XMLToTree(Config.MainTree, ltASuite1, XMLDoc);
    end;
    DeleteFile(FileName);
    Config.Changed := True;
  finally
    XMLDoc.Free;
  end;
end;

procedure LoadXMLSettings(XMLDoc: TXMLDocument);
var
  Node, tvFontStyle : IXMLNode;
begin
  if XMLDoc.Active then
  begin
    //ASuite 1.x
    Node := XMLDoc.DocumentElement.ChildNodes['Option'];
    //Get GMTheme before everything (so ASuite know where icons folder)
    Config.GMTheme        := GetStrPropertyXML(Node, 'MenuTheme','Default');
    //General
    Config.StartWithWindows   := GetBoolPropertyXML(Node, 'StartOnWindowsStartup', false);
    Config.ShowPanelAtStartUp := GetBoolPropertyXML(Node, 'StartUpShowPanel', true);
    Config.ShowGraphicMenuAtStartUp := GetBoolPropertyXML(Node, 'StartUpShowMenu', false);
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
    Config.HotKey      := GetBoolPropertyXML(Node, 'ActiveHotKey',true);
    //Window Hotkey
    Config.WindowHotkey := XMLToShortcut(Node, 'HotKeyCode', 'HotKeyModifier');
    //Menu Hotkey
    Config.GraphicMenuHotKey := XMLToShortcut(Node, 'MenuHotKeyCode', 'MenuHotKeyModifier');
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
    Config.AutorunStartup  := GetBoolPropertyXML(Node, 'ActiveAutorun',true);
    Config.AutorunShutdown := GetBoolPropertyXML(Node, 'ActiveAutorun',true);
    Config.Cache          := GetBoolPropertyXML(Node, 'ActiveCache',true);
    Config.Scheduler      := GetBoolPropertyXML(Node, 'ActiveScheduler',true);
    //Execution
    Config.ActionOnExe    := TActionOnExecute(GetIntPropertyXML(Node, 'ActionOnExe',0));
    Config.RunSingleClick := GetBoolPropertyXML(Node, 'RunSingleClick',false);
    //Trayicon
    Config.TrayCustomIconPath := GetStrPropertyXML(Node, 'TrayIconPath','');
    Config.TrayIcon           := GetBoolPropertyXML(Node, 'ActiveTrayIcon',true);
    Config.ActionClickLeft    := TTrayiconActionClick(GetIntPropertyXML(Node, 'ActionClickLeft',0));
    Config.ActionClickRight   := TTrayiconActionClick(GetIntPropertyXML(Node, 'ActionClickRight',2));
    Config.UseCustomTitle     := GetBoolPropertyXML(Node, 'ClassicMenu',false);
    //Only default menu
    Config.GMFade         := GetBoolPropertyXML(Node, 'MenuFade',true);
    Config.GMPersonalPicture := GetStrPropertyXML(Node, 'MenuPersonalPicture','Default');
  end;
end;

Function GetHotKeyCode(KeyCode: Integer) : Integer;
begin
  //TODO: Rewrite this code
  Result := -1;
  case KeyCode of
    0: Result := VkKeyScan('a');
    1: Result := VkKeyScan('b');
    2: Result := VkKeyScan('c');
    3: Result := VkKeyScan('d');
    4: Result := VkKeyScan('e');
    5: Result := VkKeyScan('f');
    6: Result := VkKeyScan('g');
    7: Result := VkKeyScan('h');
    8: Result := VkKeyScan('i');
    9: Result := VkKeyScan('j');
    10: Result := VkKeyScan('k');
    11: Result := VkKeyScan('l');
    12: Result := VkKeyScan('m');
    13: Result := VkKeyScan('n');
    14: Result := VkKeyScan('o');
    15: Result := VkKeyScan('p');
    16: Result := VkKeyScan('q');
    17: Result := VkKeyScan('r');
    18: Result := VkKeyScan('s');
    19: Result := VkKeyScan('t');
    20: Result := VkKeyScan('u');
    21: Result := VkKeyScan('v');
    22: Result := VkKeyScan('w');
    23: Result := VkKeyScan('x');
    24: Result := VkKeyScan('y');
    25: Result := VkKeyScan('z');
    26: Result := Vk_F1;
    27: Result := Vk_F2;
    28: Result := Vk_F3;
    29: Result := Vk_F4;
    30: Result := Vk_F5;
    31: Result := Vk_F6;
    32: Result := Vk_F7;
    33: Result := Vk_F8;
    34: Result := Vk_F9;
    35: Result := Vk_F10;
    36: Result := Vk_F11;
    37: Result := Vk_F12;
    38: Result := VkKeyScan('1');
    39: Result := VkKeyScan('2');
    40: Result := VkKeyScan('3');
    41: Result := VkKeyScan('4');
    42: Result := VkKeyScan('5');
    43: Result := VkKeyScan('6');
    44: Result := VkKeyScan('7');
    45: Result := VkKeyScan('8');
    46: Result := VkKeyScan('9');
    47: Result := VkKeyScan('0');
  end;
end;

Function GetHotKeyMod(KeyMod: Integer): TShiftState;
begin
  Result := [];
  case KeyMod of
    0:  Result := [ssAlt];
    1:  Result := [ssCtrl];
    2:  Result := [ssShift];
    3:  Result := [ssCtrl, ssAlt];
    4:  Result := [ssShift, ssAlt];
    5:  Result := [ssShift, ssCtrl];
    6:  Result := [ssShift, ssCtrl, ssAlt];
  end;
end;

end.
