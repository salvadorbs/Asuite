{   Component(s):
    tcyCustomWebBrowser

    Description:
    Custom webBrowser component.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyCustomWebBrowser;

interface

uses
  Classes, Graphics, SHDocVw, SysUtils, StrUtils, Windows, ActiveX, Forms, variants, MSHTML,
  {$IFDEF UNICODE}
  cyBaseWebBrowser,
  {$ELSE}
  cyBaseWebBrowserD2007,
  {$ENDIF}
  cyIEUtils;

type
  TDisplayTextSize = (dtUnknown, dtVerySmall, dtSmall, dtMedium, dtBig, dtVeryBig);

  TcyCustomWebBrowser = class(TcyBaseWebBrowser)
  private
    function GetBodyHtml: String;
    procedure SetBodyHtml(const Value: String);
    function GetBodyTextual: String;
    function GetHtml: String;
    procedure SetHtml(const Value: String);
    function GetBodyBorderStyle: TBodyBorderStyle;
    procedure SetBodyBorderStyle(const Value: TBodyBorderStyle);
    procedure SetBodyTextual(const Value: String);
    function GetSelectionText: String;
    procedure SetSelectionText(const Value: String);
    function GetDisplayTextSize: TDisplayTextSize;
    procedure SetDisplayTextSize(const Value: TDisplayTextSize);
  protected
    property BodyBorderStyle: TBodyBorderStyle read GetBodyBorderStyle write SetBodyBorderStyle;
    property BodyHtml: String read GetBodyHtml write SetBodyHtml;
    property BodyTextual: String read GetBodyTextual write SetBodyTextual;
    property Html: String read GetHtml write SetHtml;
    property SelectionText: String read GetSelectionText write SetSelectionText;
    property DisplayTextSize: TDisplayTextSize read GetDisplayTextSize write SetDisplayTextSize;
  public
    procedure CopyToBitmap(aBitmap: Graphics.TBitmap);
    procedure FindDialog;
    procedure PageSetupDialog;
    procedure Print; overload;
    procedure Print(wbPageSetup: TwbPageSetup); overload;
    procedure PrintModelessDialog;
    procedure PrintModalDialog; overload;
    procedure PrintModalDialog(wbPageSetup: TwbPageSetup); overload;
    procedure PrintPreview; overload;
    procedure PrintPreview(wbPageSetup: TwbPageSetup); overload;
    procedure ViewSource;
  end;

implementation

function TcyCustomWebBrowser.GetSelectionText: String;
var
  SelectionObject: IHTMLSelectionObject;
  Range: IDispatch;
  TxtRange: IHTMLTxtRange;
begin
  RESULT := '';

  if GetSelectionObject(SelectionObject)
  then
    try
      if SelectionObject.type_ = 'Text'
      then begin
        Range := SelectionObject.createRange;
        TxtRange := Range as IHTMLTxtRange;
        RESULT := TxtRange.Text;
      end;
    except

    end;
end;

function TcyCustomWebBrowser.GetDisplayTextSize: TDisplayTextSize;
var
  InOleVar, OutOleVar: OleVariant;
begin
  // Retrieve actual textsize :
  InOleVar := Null;
  ExecWB(OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, InOleVar, OutOleVar);

  // OutOleVar between 0 and 4 :
  case OutOleVar of
    0: RESULT := dtVerySmall;
    1: RESULT := dtSmall;
    2: RESULT := dtMedium;
    3: RESULT := dtBig;
    4: RESULT := dtVeryBig;
    else
       RESULT := dtUnknown;
  end;
end;

function TcyCustomWebBrowser.GetBodyBorderStyle: TBodyBorderStyle;
var
  Doc: IHTMLDocument2;
  Element: IHTMLElement;
begin
  RESULT := bbsSingle; // if Element.Style.BorderStyle = '', the document has a single border by default ...
  if GetIHTMLDocument2(Doc)
  then begin
    Element := Doc.Body;
    if Element <> nil
    then begin
      if Element.Style.BorderStyle = IEBodyBorderless then RESULT := bbsNone;
      if Element.Style.BorderStyle = IEBodySingleBorder then RESULT := bbsSingle;
    end;
  end;
end;

function TcyCustomWebBrowser.GetBodyHtml: String;
begin
  RESULT := SaveToString(tcBodyHTML);
end;

function TcyCustomWebBrowser.GetBodyTextual: String;
begin
  RESULT := SaveToString(tcBodyAsText);
end;

function TcyCustomWebBrowser.GetHtml: String;
begin
  RESULT := SaveToString(tcDocument);
end;

procedure TcyCustomWebBrowser.SetBodyBorderStyle(const Value: TBodyBorderStyle);
var
  Doc: IHTMLDocument2;
  Element: IHTMLElement;
begin
  if GetIHTMLDocument2(Doc)
  then begin
    Element := Doc.Body;
    if Element <> nil
    then
      case Value of
        bbsNone:   Element.Style.BorderStyle := IEBodyBorderless;
        bbsSingle: Element.Style.BorderStyle := IEBodySingleBorder;
      end;
  end;
end;

procedure TcyCustomWebBrowser.SetBodyHtml(const Value: String);
begin
  LoadFromString(Value, tcBodyHTML);
end;

procedure TcyCustomWebBrowser.SetBodyTextual(const Value: String);
begin
  LoadFromString(Value, tcBodyAsText);
end;

procedure TcyCustomWebBrowser.SetHtml(const Value: String);
begin
  LoadFromString(Value);
end;

procedure TcyCustomWebBrowser.SetSelectionText(const Value: String);
var
  SelectionObject: IHTMLSelectionObject;
  Range: IDispatch;
  TxtRange: IHTMLTxtRange;
begin
  if GetSelectionObject(SelectionObject)
  then
    try
      if SelectionObject.type_ = 'Text'
      then begin
        Range := SelectionObject.createRange;
        TxtRange := Range as IHTMLTxtRange;

        if TxtRange.Text <> Value
        then TxtRange.Text := Value;
      end;
    except

    end;
end;

procedure TcyCustomWebBrowser.SetDisplayTextSize(const Value: TDisplayTextSize);
var InOleVar, OutOleVar: OleVariant;
begin
  case Value of
    dtVerySmall: InOleVar := 0;
    dtSmall:     InOleVar := 1;
    dtMedium:    InOleVar := 2;
    dtBig:       InOleVar := 3;
    dtVeryBig:   InOleVar := 4;
    else
                 InOleVar := 2;
  end;

  OutOleVar := Null;
  ExecWB(OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, InOleVar, OutOleVar);
end;

procedure TcyCustomWebBrowser.CopyToBitmap(aBitmap: Graphics.TBitmap);
var
  viewObject: IViewObject;
  r : TRect;
begin
  if Assigned(Document)
  then begin
    Document.QueryInterface(IViewObject, viewObject);
    if Assigned(viewObject) then
    try
      r := Rect(0, 0, Width, Height);

      // Impossible to retrieve non visualized Html part ...
      aBitmap.Height := Height;
      aBitmap.Width := Width;

      viewObject.Draw(DVASPECT_CONTENT, 1, nil, nil, Forms.Application.Handle, aBitmap.Canvas.Handle, @r, nil, nil, 0) ;
    finally
      viewObject._Release;
    end;
  end;
end;

procedure TcyCustomWebBrowser.FindDialog;
const
  CGID_WebBrowser: TGUID = '{ED016940-BD5B-11cf-BA4E-00C04FD70816}';
  HTMLID_FIND = 1;
var
  CmdTarget: IOleCommandTarget;
  vaIn, vaOut: OleVariant;
  PtrGUID: PGUID;
begin
  if Assigned(Document)
  then begin
    New(PtrGUID) ;
    PtrGUID^ := CGID_WebBrowser;
    Document.QueryInterface(IOleCommandTarget, CmdTarget);

    if CmdTarget <> nil
    then
      try
        CmdTarget.Exec(PtrGUID, HTMLID_FIND, 0, vaIn, vaOut);
      finally
        CmdTarget._Release;
      end;

    Dispose(PtrGUID);
  end;
end;

procedure TcyCustomWebBrowser.ViewSource;
const
  CGID_WebBrowser: TGUID = '{ED016940-BD5B-11cf-BA4E-00C04FD70816}';
  HTMLID_VIEWSOURCE = 2;
var
  CmdTarget: IOleCommandTarget;
  vaIn, vaOut: OleVariant;
  PtrGUID: PGUID;
begin
  if Assigned(Document)
  then begin
    New(PtrGUID) ;
    PtrGUID^ := CGID_WebBrowser;

    Document.QueryInterface(IOleCommandTarget, CmdTarget) ;
    if CmdTarget <> nil
    then
      try
        CmdTarget.Exec(PtrGUID, HTMLID_VIEWSOURCE, 0, vaIn, vaOut) ;
      finally
        CmdTarget._Release;
      end;

    Dispose(PtrGUID);
  end;
end;

procedure TcyCustomWebBrowser.Print;
var
  vIn, vOut: OleVariant;
  PrintOption: Cardinal;
begin
  PrintOption := OLECMDEXECOPT_DONTPROMPTUSER;
  ControlInterface.ExecWB(OLECMDID_PRINT, PrintOption, vIn, vOut);
end;

procedure TcyCustomWebBrowser.Print(wbPageSetup: TwbPageSetup);
var SavePageSetup: TwbPageSetup;
begin
  // Backup PageSetup :
  GetPageSetupFromRegistry(SavePageSetup);
  // Set PageSetup :
  SetPageSetupToRegistry(wbPageSetup);

  try
    Print;
  finally

  end;

  // Restore Backup :
  SetPageSetupToRegistry(SavePageSetup);
end;

procedure TcyCustomWebBrowser.PrintModelessDialog;
var
  HTMLWnd: IHTMLWindow2;
  HTMLWindow3: IHTMLWindow3;

  vIn, vOut: OleVariant;
  PrintOption: Cardinal;
begin
  PrintOption := OLECMDEXECOPT_PROMPTUSER;
  ControlInterface.ExecWB(OLECMDID_PRINT, PrintOption, vIn, vOut);
end;

procedure TcyCustomWebBrowser.PrintModalDialog;
var
  Doc: IHTMLDocument2;
  HTMLWnd: IHTMLWindow2;
  HTMLWindow3: IHTMLWindow3;
begin
  if GetIHTMLDocument2(Doc)
  then begin
    HTMLWnd := Doc.ParentWindow;
    HTMLWindow3 := HTMLWnd as IHTMLWindow3;
    HTMLWindow3.print;
  end;
end;

procedure TcyCustomWebBrowser.PrintModalDialog(wbPageSetup: TwbPageSetup);
var SavePageSetup: TwbPageSetup;
begin
  // Backup PageSetup :
  GetPageSetupFromRegistry(SavePageSetup);
  // Set PageSetup :
  SetPageSetupToRegistry(wbPageSetup);

  try
    PrintModalDialog;
  finally

  end;

  // Restore Backup :
  SetPageSetupToRegistry(SavePageSetup);
end;

procedure TcyCustomWebBrowser.PrintPreview;
var vIn, vOut: OleVariant;
begin
  ControlInterface.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DONTPROMPTUSER, vIn, vOut);
end;

procedure TcyCustomWebBrowser.PrintPreview(wbPageSetup: TwbPageSetup);
var SavePageSetup: TwbPageSetup;
begin
  // Backup PageSetup :
  GetPageSetupFromRegistry(SavePageSetup);
  // Set PageSetup :
  SetPageSetupToRegistry(wbPageSetup);

  try
    PrintPreview;
  finally

  end;

  // Restore Backup :
  SetPageSetupToRegistry(SavePageSetup);
end;

procedure TcyCustomWebBrowser.PageSetupDialog;
var vIn, vOut: OleVariant;
begin
  ControlInterface.ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_PROMPTUSER, vIn, vOut);
end;

end.
