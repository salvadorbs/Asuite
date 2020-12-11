unit Utility.XML;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, VirtualTrees, Kernel.Consts,
  DOM, XMLRead, Kernel.Enumerations;

{ Load list or settings from xml file }
type
  TImportListToTree = function(Tree: TVirtualStringTree;Node: TDOMNode;Parent: PVirtualNode): PVirtualNode of object;

  TImportOldListProcs = class
    class function ASuite1NodeToTree(Tree: TVirtualStringTree;XMLNode: TDOMNode;
                                 Parent: PVirtualNode): PVirtualNode;
    class function wppLauncherNodeToTree(Tree: TVirtualStringTree;XMLNode: TDOMNode;
                                     Parent: PVirtualNode): PVirtualNode;
    class function PStartNodeToTree(Tree: TVirtualStringTree;XMLNode: TDOMNode;
                                Parent: PVirtualNode): PVirtualNode;
  end;

procedure XMLToTree(Tree: TVirtualStringTree;ListType: TListType;
                    XMLDoc: TXMLDocument);
procedure LoadDatabaseFromXML(FileName: string);
procedure LoadXMLSettings(XMLDoc: TXMLDocument);
function XMLToShortcut(Node: TDOMNode; AFieldCode, AFieldMod: string): TShortcut;
Function GetHotKeyCode(KeyCode: Integer): Integer;
Function GetHotKeyMod(KeyMod: Integer): TShiftState;

{ Methods to get values }
function GetStrPropertyXML(Node: TDOMNode; Name: String; Default: String): String;
function GetIntPropertyXML(Node: TDOMNode; Name: String; Default: Integer): Integer;
function GetBoolPropertyXML(Node: TDOMNode; Name: String; Default: Boolean): Boolean;

implementation

uses
  Forms.Main, NodeDataTypes.Custom, Utility.Misc, AppConfig.Main, Kernel.Types,
  Utility.Conversions, VirtualTree.Methods, NodeDataTypes.Files, Menus,
  Kernel.Logger{$IFDEF MSWINDOWS} , Windows {$ENDIF};

function GetStrPropertyXML(Node : TDOMNode;Name: String;Default: String): String;
var
  PropertyNode: TDOMNode;
begin
  Result := Default;
  PropertyNode := Node.FindNode(Name);
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.NodeValue <> '' then
      Result := PropertyNode.NodeValue;
end;

function GetIntPropertyXML(Node : TDOMNode;Name: String;Default: Integer): Integer;
var
  PropertyNode: TDOMNode;
begin
  Result := Default;
  PropertyNode := Node.FindNode(Name);
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.NodeValue <> '' then
      Result := StrToInt(PropertyNode.NodeValue);
end;

function GetBoolPropertyXML(Node : TDOMNode;Name: String;Default: Boolean): Boolean;
var
  PropertyNode: TDOMNode;
begin
  Result := Default;
  PropertyNode := Node.FindNode(Name);
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.NodeValue <> '' then
      Result := Utility.Conversions.StrToBool(PropertyNode.NodeValue);
end;

class function TImportOldListProcs.ASuite1NodeToTree(Tree: TVirtualStringTree;XMLNode: TDOMNode;
                                          Parent: PVirtualNode): PVirtualNode;
var
  NodeData : PBaseData;
  schDate, schTime, sName : string;
  CustomRealNodeData : TvCustomRealNodeData;
begin
  Result := nil;
  if ((XMLNode.HasAttributes) or (XMLNode.NodeName = 'Separator')) and
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
      //Get name
      if XMLNode.HasAttributes and (XMLNode.Attributes.Length > 0) then
        sName := XMLNode.Attributes[0].NodeValue
      else
        sName := '';

      CustomRealNodeData.Name     := sName;
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

function XMLToShortcut(Node: TDOMNode; AFieldCode, AFieldMod: string): TShortcut;
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
  XMLNode: TDOMNode; Parent: PVirtualNode): PVirtualNode;
var
  NodeData     : PBaseData;
  sName: string;
  CustomRealNodeData : TvCustomRealNodeData;
begin
  Result := nil;
  if ((XMLNode.HasAttributes) or (XMLNode.NodeName = 'separator')) and
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
      //Get name
      if XMLNode.HasAttributes and (XMLNode.Attributes.Length > 0) then
        sName := XMLNode.Attributes[0].NodeValue
      else
        sName := '';

      CustomRealNodeData.Name     := sName;
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

class function TImportOldListProcs.wppLauncherNodeToTree(Tree: TVirtualStringTree;XMLNode: TDOMNode;
                                               Parent: PVirtualNode): PVirtualNode;
var
  NodeData     : PBaseData;
  sName: string;
  CustomRealNodeData : TvCustomRealNodeData;
begin
  Result := nil;
  if ((XMLNode.HasAttributes) or (XMLNode.NodeName = 'separator')) and
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
      //Get name
      if XMLNode.HasAttributes and (XMLNode.Attributes.Length > 0) then
        sName := XMLNode.Attributes[0].NodeValue
      else
        sName := '';

      CustomRealNodeData.Name     := sName;
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
  cXMLNode : TDOMNode;
  AConvMethod: TImportListToTree;

  procedure ProcessNode(XMLNode : TDOMNode;TreeNode : PVirtualNode);
  var
    cNode : TDOMNode;
  begin
    if XMLNode = nil then Exit;

    //Import xml node in vstListImp
    TreeNode := AConvMethod(Tree, XMLNode, TreeNode);

    //Next nodes
    cNode := XMLNode.FirstChild;
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
      TASuiteLogger.Info('Found ASuite 1.x List (%s)', [XMLDoc.documentURI]);
      AConvMethod := TImportOldListProcs.ASuite1NodeToTree;
    end;
    ltwppLauncher1:
    begin
      TASuiteLogger.Info('Found winPenPack Launcher 1.x List (%s)', [XMLDoc.documentURI]);
      AConvMethod := TImportOldListProcs.wppLauncherNodeToTree;
    end;
    ltPStart1:
    begin
      TASuiteLogger.Info('Found PStart 1.x List (%s)', [XMLDoc.documentURI]);
      AConvMethod := TImportOldListProcs.PStartNodeToTree;
    end;
  end;

  Tree.Clear;
  Tree.BeginUpdate;
  try
    cXMLNode := XMLDoc.DocumentElement.FirstChild;

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
  try
    ReadXMLFile(XMLDoc, FileName);
    //Load list and settings
    if (XMLDoc.DocumentElement.NodeName = 'ASuite') then
    begin
      LoadXMLSettings(XMLDoc);
      XMLToTree(Config.MainTree, ltASuite1, XMLDoc);
    end;
    SysUtils.DeleteFile(FileName);
    Config.Changed := True;
  finally
    XMLDoc.Free;
  end;
end;

procedure LoadXMLSettings(XMLDoc: TXMLDocument);
var
  Node, tvFontStyle : TDOMNode;
begin
  if Assigned(XMLDoc.DocumentElement) then
  begin
    //ASuite 1.x
    Node := XMLDoc.DocumentElement.FindNode('Option');
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
    tvFontStyle             := Node.FindNode('TreeViewFontStyle');
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
  Result := 0;
  case KeyCode of
    0: Result := VK_A;
    1: Result := VK_b;
    2: Result := VK_c;
    3: Result := VK_d;
    4: Result := VK_e;
    5: Result := VK_f;
    6: Result := VK_g;
    7: Result := VK_h;
    8: Result := VK_i;
    9: Result := VK_j;
    10: Result := VK_k;
    11: Result := VK_l;
    12: Result := VK_m;
    13: Result := VK_n;
    14: Result := VK_o;
    15: Result := VK_p;
    16: Result := VK_q;
    17: Result := VK_r;
    18: Result := VK_s;
    19: Result := VK_t;
    20: Result := VK_u;
    21: Result := VK_v;
    22: Result := VK_w;
    23: Result := VK_x;
    24: Result := VK_y;
    25: Result := VK_z;
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
    38: Result := VK_1;
    39: Result := VK_2;
    40: Result := VK_3;
    41: Result := VK_4;
    42: Result := VK_5;
    43: Result := VK_6;
    44: Result := VK_7;
    45: Result := VK_8;
    46: Result := VK_9;
    47: Result := VK_0;
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
