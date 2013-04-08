{Version 10.00}
{*********************************************************}
{*                     FRAMBRWZ.PAS                      *}
{*********************************************************}
{
Copyright (c) 1995-2008 by L. David Baldwin

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules, HTMLGIF1.PAS, PNGZLIB1.PAS, DITHERUNIT.PAS, and
URLCON.PAS are covered by separate copyright notices located in those modules.
}

unit FramBrwz;

{$I htmlcons.inc}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SysUtils, Windows, Classes, Messages, Controls, ExtCtrls,
  htmlsubs, htmlview, htmlun2, readHTML, UrlSubs, FramView;

type
  TGetPostRequestEvent = procedure(Sender: TObject; IsGet: boolean; const URL, Query: string;
    Reload: boolean; var NewURL: string; var DocType: ThtmlFileType; var Stream: TMemoryStream) of object;

  TGetPostRequestExEvent = procedure(Sender: TObject; IsGet: boolean; const URL, Query, EncType, Referer: string;
    Reload: boolean; var NewURL: string; var DocType: ThtmlFileType; var Stream: TMemoryStream) of object;
    
  TbrFormSubmitEvent = procedure(Sender: TObject; Viewer: ThtmlViewer;
    const Action, Target, EncType, Method: string; Results: TStringList; var Handled: boolean) of object;

  TFrameBaseOpener = class(TFrameBase);

  TbrFrameSet = class;
  TbrSubFrameSet = class;

  TbrFrame = class(TViewerFrameBase) {TbrFrame holds a ThtmlViewer or TbrSubFrameSet}
  private
    URLBase: string;
  protected
    function ExpandSourceName(Base, Path: string; S: string): string; override;
    function FrameSet: TbrSubFrameSet; {$ifdef HASINLINE}inline;{$endif}
    function GetSubFrameSetClass: TSubFrameSetClass; override;
    function MasterSet: TbrFrameSet; {$ifdef HASINLINE}inline;{$endif}
    procedure CreateViewer; override;
    procedure frLoadFromBrzFile(const URL, Dest, Query, EncType, Referer: string; Bump, IsGet, Reload: boolean);
    procedure LoadFiles; override;
    procedure RefreshEvent(Sender: TObject; Delay: integer; const URL: string); override;
    procedure RefreshTimerTimer(Sender: TObject); override;
    procedure ReLoadFiles(APosition: LongInt); override;
    procedure URLExpandName(Sender: TObject; const SRC: string; var Rslt: string);
  protected
    TheStream: TMemoryStream;
    TheStreamType: ThtmlFileType;
  end;

  TbrSubFrameSet = class(TSubFrameSetBase) {can contain one or more TbrFrames and/or TSubFrameSets}
  private
    URLBase: string;
  protected
    function GetFrameClass: TViewerFrameClass; override;
    procedure RefreshTimerTimer(Sender: Tobject); override;
  public
    constructor CreateIt(AOwner: TComponent; Master: TFrameSetBase); override;
  end;

  TFrameBrowser = class;

  TbrFrameSet = class(TFrameSetBase) {only one of these showing, others may be held as History}
  private
    URLBase: string;
  protected
    function FrameViewer: TFrameBrowser; {$ifdef HASINLINE}inline;{$endif}
    function GetFrameClass: TViewerFrameClass; override;
    function MasterSet: TbrFrameSet; {$ifdef HASINLINE}inline;{$endif}
    function RequestEvent: Boolean; override;
    procedure RefreshTimerTimer(Sender: TObject); override;
    procedure LoadFromBrzFile(Stream: TMemoryStream; StreamType: ThtmlFileType; const URL, Dest: string);
  end;

  TFrameBrowser = class(TFVBase)
  private
    FOnFormSubmit: TbrFormSubmitEvent;
    FOnGetPostRequest: TGetPostRequestEvent;
    FOnGetPostRequestEx: TGetPostRequestExEvent;
    FEncodePostArgs: boolean;
    InFormSubmit: boolean;
    function CurbrFrameSet: TbrFrameSet; {$ifdef HASINLINE}inline;{$endif} {the TbrFrameSet being displayed}
    function HotSpotClickHandled(const FullUrl: string): boolean;
    procedure LoadURLInternal(const URL, Query, EncType, Referer: string; IsGet, Reload: boolean);
  protected
    function GetFrameSetClass: TFrameSetClass; override;
    function GetSubFrameSetClass: TSubFrameSetClass; override;
    procedure HotSpotCovered(Sender: TObject; const SRC: string); override;
    procedure CheckVisitedLinks; override;
    procedure DoFormSubmitEvent(Sender: TObject; const Action, Target, EncType, Method: string; Results: TStringList); override;
    procedure DoURLRequest(Sender: TObject; const SRC: string; var Stream: TMemoryStream); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetViewerUrlBase(Viewer: ThtmlViewer): string;
    procedure GetPostQuery(const URL, Query, EncType: string; IsGet: boolean);
    procedure HotSpotClick(Sender: TObject; const AnURL: string; var Handled: boolean); override;
    procedure LoadURL(const URL: string);
    property EncodePostArgs: boolean read FEncodePostArgs write FEncodePostArgs;
  published
    property OnGetPostRequest: TGetPostRequestEvent read FOnGetPostRequest write FOnGetPostRequest;
    property OnGetPostRequestEx: TGetPostRequestExEvent read FOnGetPostRequestEx write FOnGetPostRequestEx;
    property OnFormSubmit: TbrFormSubmitEvent read FOnFormSubmit write FOnFormSubmit;
  end;

implementation

const
  Sequence: integer = 10;

function StreamToString(Stream: TStream): string;
var
  SL: TStringList;
begin
  Result := '';
  try
    SL := TStringList.Create;
    try
      SL.LoadFromStream(Stream);
      Result := SL.Text;
    finally
      Stream.Position := 0;
      SL.Free;
    end;
  except
  end;
end;

{----------------SplitURL}

procedure SplitURL(const Src: string; var FName, Dest: string);
{Split an URL into filename and Destination}
var
  I: integer;
begin
  I := Pos('#', Src);
  if I >= 1 then
  begin
    Dest := System.Copy(Src, I, Length(Src) - I + 1); {local destination}
    FName := System.Copy(Src, 1, I - 1); {the file name}
  end
  else
  begin
    FName := Src;
    Dest := ''; {no local destination}
  end;
end;

function ConvDosToHTML(const Name: string): string; forward;

{----------------TbrFrame.CreateIt}

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TbrFrame.ExpandSourceName(Base, Path: string; S: string): string;
begin
  S := ConvDosToHTML(S);
  if Pos(':/', S) <> 0 then
    URLBase := URLSubs.GetBase(S) {get new base}
  else if Base <> '' then
  begin
    S := Combine(Base, S);
    URLBase := Base;
  end
  else
  begin
    if LOwner is TbrFrameSet then
      URLBase := (LOwner as TbrFrameSet).URLBase
    else
      URLBase := (LOwner as TbrSubFrameSet).URLBase;
    S := Combine(URLBase, S);
  end;
  Result := S
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrame.FrameSet: TbrSubFrameSet;
begin
  Result := TbrSubFrameSet(inherited FrameSet);
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrame.GetSubFrameSetClass: TSubFrameSetClass;
begin
  Result := TbrSubFrameSet;
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TbrFrame.MasterSet: TbrFrameSet;
begin
  Result := TbrFrameSet(inherited MasterSet);
end;

procedure TbrFrame.RefreshEvent(Sender: TObject; Delay: integer; const URL: string);
var
  Ext: string;
begin
  if not (fvMetaRefresh in MasterSet.FrameViewer.fvOptions) then
    Exit;
  Ext := Lowercase(GetURLExtension(URL));
  if (Ext = 'exe') or (Ext = 'zip') then
    Exit;
  if URL = '' then
    NextFile := Source
  else if not IsFullURL(URL) then
    NextFile := Combine(URLBase, URL) //URLBase + URL
  else
    NextFile := URL;
  if not Assigned(RefreshTimer) then
    RefreshTimer := TTimer.Create(Self);
  RefreshTimer.OnTimer := RefreshTimerTimer;
  RefreshTimer.Interval := Delay * 1000;
  RefreshTimer.Enabled := True;
end;

procedure TbrFrame.RefreshTimerTimer(Sender: TObject);
var
  S, D: string;
begin
  RefreshTimer.Enabled := False;
  if Unloaded then
    Exit;
  if not IsFullUrl(NextFile) then
    NextFile := Combine(UrlBase, NextFile);
  if (MasterSet.Viewers.Count = 1) then {load a new FrameSet}
    MasterSet.FrameViewer.LoadURLInternal(NextFile, '', '', '', True, True)
  else
  begin
    SplitURL(NextFile, S, D);
    frLoadFromBrzFile(S, D, '', '', '', True, True, True);
  end;
end;

procedure TbrFrame.CreateViewer;
begin
  inherited CreateViewer;
  Viewer.OnExpandName := UrlExpandName;
end;

{----------------TbrFrame.LoadFiles}

procedure TbrFrame.LoadFiles;
var
  Item: TFrameBaseOpener;
  I: integer;
  Upper, Lower: boolean;
  Msg: string;
  NewURL: string;
  TheString: string;
begin
  if (Source <> '') and (MasterSet.NestLevel < 4) then
  begin
    if not Assigned(TheStream) then
    begin
      NewURL := '';
      if Assigned(MasterSet.FrameViewer.FOnGetPostRequestEx) then
        MasterSet.FrameViewer.FOnGetPostRequestEX(Self, True, Source, '', '', '', False, NewURL, TheStreamType, TheStream)
      else
        MasterSet.FrameViewer.FOnGetPostRequest(Self, True, Source, '', False, NewURL, TheStreamType, TheStream);
      if NewURL <> '' then
        Source := NewURL;
    end;
    URLBase := GetBase(Source);
    Inc(MasterSet.NestLevel);
    try
      TheString := StreamToString(TheStream);
      if (TheStreamType = HTMLType) and IsFrameString(LsString, '', TheString,
        MasterSet.FrameViewer) then
      begin
        FFrameSet := TbrSubFrameSet.CreateIt(Self, MasterSet);
        FrameSet.Align := alClient;
        FrameSet.Visible := False;
        InsertControl(FrameSet);
        FrameSet.SendToBack;
        FrameSet.Visible := True;
        FrameParseString(MasterSet.FrameViewer, FrameSet, lsString, '', TheString, FrameSet.HandleMeta);
        Self.BevelOuter := bvNone;
        frBumpHistory1(Source, 0);
        with FrameSet do
        begin
          for I := 0 to List.Count - 1 do
          begin
            Item := TFrameBaseOpener(List.Items[I]);
            Item.LoadFiles;
          end;
          CheckNoresize(Lower, Upper);
          if FRefreshDelay > 0 then
            SetRefreshTimer;
        end;
      end
      else
      begin
        CreateViewer;
        Viewer.Base := MasterSet.FBase;
        Viewer.LoadStream(Source, TheStream, TheStreamType);
        Viewer.PositionTo(Destination);
        frBumpHistory1(Source, Viewer.Position);
      end;
    except
      if not Assigned(Viewer) then
        CreateViewer;
      FreeAndNil(FFrameSet);
      Msg := '<p><img src="qw%&.bmp" alt="Error"> Can''t load ' + Source;
      Viewer.LoadFromBuffer(@Msg[1], Length(Msg), ''); {load an error message}
    end;
    Dec(MasterSet.NestLevel);
  end
  else
  begin {so blank area will perform like the TFrameBrowser}
    OnMouseDown := MasterSet.FrameViewer.OnMouseDown;
    OnMouseMove := MasterSet.FrameViewer.OnMouseMove;
    OnMouseUp := MasterSet.FrameViewer.OnMouseUp;
  end;
end;

{----------------TbrFrame.ReloadFiles}

procedure TbrFrame.ReloadFiles(APosition: LongInt);
var
  Item: TFrameBaseOpener;
  I: integer;
  Upper, Lower: boolean;
  Dummy: string;

  procedure DoError;
  var
    Msg: string;
  begin
    Msg := '<p><img src="qw%&.bmp" alt="Error"> Can''t load ' + Source;
    Viewer.LoadFromBuffer(@Msg[1], Length(Msg), ''); {load an error message}
  end;

begin
  if Source <> '' then
    if FrameSet <> nil then
    begin
      with FrameSet do
      begin
        for I := 0 to List.Count - 1 do
        begin
          Item := TFrameBaseOpener(List.Items[I]);
          Item.ReloadFiles(APosition);
        end;
        CheckNoresize(Lower, Upper);
      end;
    end
    else if Assigned(Viewer) then
    begin
      Viewer.Base := MasterSet.FBase; {only effective if no Base to be read}
      try
        if Assigned(MasterSet.FrameViewer.FOnGetPostRequestEx) then
          MasterSet.FrameViewer.FOnGetPostRequestEx(Self, True, Source, '', '', '', False,
            Dummy, TheStreamType, TheStream)
        else
          MasterSet.FrameViewer.FOnGetPostRequest(Self, True, Source, '', False,
            Dummy, TheStreamType, TheStream);
        Viewer.LoadStream(Source, TheStream, TheStreamType);
        if APosition < 0 then
          Viewer.Position := ViewerPosition
        else
          Viewer.Position := APosition; {its History Position}
        Viewer.FormData := ViewerFormData;
        ViewerFormData.Free;
        ViewerFormData := nil;
      except
        DoError;
      end;
    end;
  Unloaded := False;
end;

{----------------TbrFrame.frLoadFromBrzFile}

procedure TbrFrame.frLoadFromBrzFile(const URL, Dest, Query, EncType, Referer: string; Bump, IsGet, Reload: boolean);
{URL is full URL here, has been seperated from Destination}
var
  OldPos: LongInt;
  HS, S, S1, OldTitle, OldName, OldBase: string;
  OldFormData: TFreeList;
  SameName: boolean;
  OldViewer: ThtmlViewer;
  OldFrameSet: TbrSubFrameSet;
  TheString: string;
  Upper, Lower, FrameFile: boolean;
  Item: TFrameBase;
  I: integer;

begin
  if Assigned(RefreshTimer) then
    RefreshTimer.Enabled := False;
  OldName := Source;
  OldBase := URLBase;
  S := URL;
  if S = '' then
    S := OldName
  else
    URLBase := URLSubs.GetBase(S); {get new base}
  HS := S;
  SameName := CompareText(S, OldName) = 0;
{if SameName, will not have to reload anything unless Reload set}

  if not SameName or Reload then
  begin
    if Assigned(Viewer) and Assigned(MasterSet.FrameViewer.OnViewerClear) then
      MasterSet.FrameViewer.OnViewerClear(Viewer);
    S1 := '';
    if Assigned(MasterSet.FrameViewer.OnGetPostRequestEx) then
      MasterSet.FrameViewer.OnGetPostRequestEx(Self, IsGet, S, Query, EncType, Referer, Reload, S1, TheStreamType, TheStream)
    else
      MasterSet.FrameViewer.OnGetPostRequest(Self, IsGet, S, Query, Reload, S1, TheStreamType, TheStream);
    if S1 <> '' then
    begin
      S := S1;
      URLBase := GetBase(S);
    end;
  end;
  Source := S;

  try
    TheString := StreamToString(TheStream);
    if not SameName then
    try
      FrameFile := (TheStreamType = HTMLType) and
        IsFrameString(lsString, '', TheString, MasterSet.FrameViewer);
    except
      raise(EfvLoadError.Create('Can''t load: ' + URL));
    end
    else
      FrameFile := not Assigned(Viewer);
    if SameName and not Reload then
      if Assigned(Viewer) then
      begin
        OldPos := Viewer.Position;
        Viewer.PositionTo(Dest);
        MasterSet.FrameViewer.AddVisitedLink(URL + Dest);
        if Bump and (Viewer.Position <> OldPos) then
        {Viewer to Viewer}
          frBumpHistory(HS, Viewer.Position, OldPos, nil);
      end
      else
      begin
        with FrameSet do
          for I := 0 to List.Count - 1 do
          begin
            Item := TFrameBase(List.Items[I]);
            if (Item is TbrFrame) then
              with TbrFrame(Item) do
                if CompareText(Source, OrigSource) <> 0 then
                  frLoadFromBrzFile(OrigSource, '', '', '', '', True, True, False);
          end;
        Exit;
      end
    else if Assigned(Viewer) and not FrameFile then {not samename or samename and reload}
    begin {Viewer already assigned and it's not a Frame file}
      OldPos := Viewer.Position;
      OldTitle := Viewer.DocumentTitle;
      if Bump and not SameName and (MasterSet.Viewers.Count > 1) then
        OldFormData := Viewer.FormData
      else
        OldFormData := nil;
      try
        Viewer.Base := MasterSet.FBase;
        Viewer.LoadStream(Source, TheStream, TheStreamType);
        if (Dest <> '') then
          Viewer.PositionTo(Dest);
        MasterSet.FrameViewer.AddVisitedLink(URL + Dest);
        if not samename then
        begin {don't bump history on a forced reload}
          if MasterSet.Viewers.Count > 1 then
          begin
            if Bump then
             {Viewer to Viewer}
              frBumpHistory(HS, Viewer.Position, OldPos, OldFormData)
            else
              OldFormData.Free;
          end
          else if (MasterSet.Viewers.Count = 1) and Bump then
          {a single viewer situation, bump the history here}
            with MasterSet do
            begin
              FCurrentFile := Source;
              FTitle := Viewer.DocumentTitle;
              FBase := Viewer.Base;
              FBaseTarget := Viewer.BaseTarget;
              FrameViewer.BumpHistory1(OldName, OldTitle, OldPos, HTMLType);
            end;
        end;
      except
        OldFormData.Free;
        raise;
      end;
    end
    else
    begin {Viewer is not assigned or it is a Frame File}
    {keep the old viewer or frameset around (free later) to minimize blink}
      OldViewer := Viewer;       FViewer := nil;
      OldFrameSet := FrameSet; FFrameSet := nil;
      if OldFrameSet <> nil then
        OldFrameSet.ClearFrameNames;
      if FrameFile then
      begin {it's a frame file}
        FFrameSet := TbrSubFrameSet.CreateIt(Self, MasterSet);
        FrameSet.URLBase := URLBase;
        FrameSet.Align := alClient;
        FrameSet.Visible := False;
        InsertControl(FrameSet);
        FrameSet.SendToBack; {to prevent blink}
        FrameSet.Visible := True;
        FrameParseString(MasterSet.FrameViewer, FrameSet, lsString, '', TheString, FrameSet.HandleMeta);
        MasterSet.FrameViewer.AddVisitedLink(URL);
        Self.BevelOuter := bvNone;
        with FrameSet do
        begin
          for I := 0 to List.Count - 1 do
            TFrameBaseOpener(List.Items[I]).LoadFiles;
          CheckNoresize(Lower, Upper);
          if FRefreshDelay > 0 then
            SetRefreshTimer;
        end;
        if Assigned(OldViewer) then
          frBumpHistory(HS, 0, OldViewer.Position, OldViewer.FormData)
        else
          frBumpHistory(S, 0, 0, nil);
      end
      else
      begin {not a frame file but needs a viewer}
        CreateViewer;
        Viewer.Base := MasterSet.FBase;
        Viewer.LoadStream(Source, TheStream, TheStreamType);
        Viewer.PositionTo(Dest);
        MasterSet.FrameViewer.AddVisitedLink(URL + Dest);
      {FrameSet to Viewer}
        frBumpHistory(HS, Viewer.Position, 0, nil);
      end;
      if FrameSet <> nil then
        with FrameSet do
        begin
          with ClientRect do
            InitializeDimensions(Left, Top, Right - Left, Bottom - Top);
          CalcSizes(nil);
        end;
      if Assigned(Viewer) then
      begin
        if MasterSet.BorderSize = 0 then
          BevelOuter := bvNone
        else
        begin
          BevelOuter := bvLowered;
          BevelWidth := MasterSet.BorderSize;
        end;
        if (Dest <> '') then
          Viewer.PositionTo(Dest);
      end;
      if Assigned(OldViewer) then
      begin
        MasterSet.Viewers.Remove(OldViewer);
        if MasterSet.FActive = OldViewer then
          MasterSet.FActive := nil;
        OldViewer.Free;
      end
      else if Assigned(OldFrameSet) then
      begin
        OldFrameSet.UnloadFiles;
        OldFrameSet.Visible := False;
      end;
      RePaint;
    end;
  except
    Source := OldName;
    URLBase := OldBase;
    raise;
  end;
end;

function ConvDosToHTML(const Name: string): string;
{if Name is a Dos filename, convert it to HTML.  Add the file:// if it is
 a full pathe filename}
begin
  Result := Name;
  if Pos('\', Result) > 0 then
  begin
    Result := DosToHTML(Result);
    if (Pos('|', Result) > 0) then {was something like c:\....}
      Result := 'file:///' + Result;
  end;
end;

{----------------TbrFrame.URLExpandName}

procedure TbrFrame.URLExpandName(Sender: TObject; const SRC: string; var Rslt: string);
var
  S: string;
  Viewer: ThtmlViewer;
begin
  S := ConvDosToHTML(SRC);
  if not IsFullUrl(S) then
  begin
    Viewer := Sender as ThtmlViewer;
    if Viewer.Base <> '' then
      Rslt := Combine(GetBase(ConvDosToHTML(Viewer.Base)), S)
    else
      Rslt := Combine(UrlBase, S);
  end
  else
    Rslt := S;
end;

{----------------TbrSubFrameSet.CreateIt}

constructor TbrSubFrameSet.CreateIt(AOwner: TComponent; Master: TFrameSetBase);
begin
  inherited CreateIt(AOwner, Master);
  if (AOwner is TbrFrame) then
    URLBase := TbrFrame(AOwner).URLBase
  else if (AOwner is TbrFrameSet) then
    URLBase := TbrFrameSet(AOwner).URLBase
  else if (AOwner is TbrSubFrameSet) then
    URLBase := TbrSubFrameSet(AOwner).URLBase;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrSubFrameSet.GetFrameClass: TViewerFrameClass;
begin
  Result := TbrFrame;
end;

{----------------TbrSubFrameSet.RefreshTimerTimer}

procedure TbrSubFrameSet.RefreshTimerTimer(Sender: Tobject);
var
  S, D: string;
begin
  RefreshTimer.Enabled := False;
  if Unloaded then
    Exit;
  if Owner is TbrFrame then
  begin
    SplitURL(NextFile, S, D);
    TbrFrame(Owner).frLoadFromBrzFile(S, D, '', '', '', True, True, True)
  end;
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TbrFrameSet.FrameViewer: TFrameBrowser;
begin
  Result := TFrameBrowser(inherited FrameViewer);
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrameSet.GetFrameClass: TViewerFrameClass;
begin
  Result := TbrFrame;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrameSet.MasterSet: TbrFrameSet;
begin
  Result := TbrFrameSet(inherited MasterSet);
end;

procedure TbrFrameSet.RefreshTimerTimer(Sender: Tobject);
begin
  RefreshTimer.Enabled := False;
  if (Self = MasterSet.FrameViewer.CurFrameSet) then
    FrameViewer.LoadURLInternal(NextFile, '', '', '', True, True)
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrameSet.RequestEvent: Boolean;
begin
  Result := True;
end;

{----------------TbrFrameSet.LoadFromBrzFile}

procedure TbrFrameSet.LoadFromBrzFile(Stream: TMemoryStream; StreamType: ThtmlFileType;
  const URL, Dest: string);
var
  I: integer;
  Frame: TbrFrame;
  Lower, Upper: boolean;
  TheString: string;
begin
  Clear;
  NestLevel := 0;
  FCurrentFile := URL;
  TheString := StreamToString(Stream);
  if (StreamType = HTMLType) and
    IsFrameString(lsString, '', TheString, MasterSet.FrameViewer) then
  begin {it's a Frameset html file}
    FrameParseString(FrameViewer, Self, lsString, '', TheString, HandleMeta);
    for I := 0 to List.Count - 1 do
      TFrameBaseOpener(List.Items[I]).LoadFiles;
    CalcSizes(Self);
    CheckNoresize(Lower, Upper);
    if FRefreshDelay > 0 then
      SetRefreshTimer;
  end
  else
  begin {it's a non frame file}
    Frame := TbrFrame(AddFrame(nil, ''));
    Frame.Source := URL;
    Frame.TheStream := Stream;
    Frame.TheStreamType := StreamType;
    Frame.Destination := Dest;
    EndFrameSet;
    CalcSizes(Self);
    Frame.LoadFiles;
    FTitle := ReadHTML.Title;
    FBase := ReadHTML.Base;
    FBaseTarget := ReadHTML.BaseTarget;
  end;
end;

{----------------TFrameBrowser.Create}

constructor TFrameBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FEncodePostArgs := True;
end;

{----------------TFrameBrowser.LoadURL}

procedure TFrameBrowser.LoadURL(const URL: string);
begin
  if not Processing then
  begin
    LoadURLInternal(Normalize(URL), '', '', '', True, False);
  end;
end;

{----------------TFrameBrowser.GetPostQuery}

procedure TFrameBrowser.GetPostQuery(const URL, Query, EncType: string; IsGet: boolean);
begin
  if not Processing then
    LoadURLInternal(Normalize(URL), Query, EncType, '', IsGet, True);
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
function TFrameBrowser.GetFrameSetClass: TFrameSetClass;
begin
  Result := TbrFrameSet;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TFrameBrowser.GetSubFrameSetClass: TSubFrameSetClass;
begin
  Result := TbrSubFrameSet;
end;

{----------------TFrameBrowser.LoadURLInternal}

procedure TFrameBrowser.LoadURLInternal(const URL, Query, EncType, Referer: string;
  IsGet, Reload: boolean);
var
  OldFrameSet: TbrFrameSet;
  OldFile, S, Dest, S1: string;
  OldPos: LongInt;
  Tmp: TObject;
  SameName: boolean;
{$IFDEF Windows}
  Dummy: integer;
{$ENDIF}
  Stream: TMemoryStream;
  StreamType: ThtmlFileType;
  I: integer;
begin
  if not Assigned(FOnGetPostRequest) and not Assigned(FOnGetPostRequestEx) then
    raise(Exception.Create('No OnGetPostRequest or OnGetPostRequestEx event defined'));
  BeginProcessing;
{$IFDEF windows}
  Dummy :=
{$ENDIF}
  IOResult; {remove any pending file errors}
  SplitURL(URL, S, Dest);
  try
    OldFile := CurbrFrameSet.FCurrentFile;
    ProcessList.Clear;
    if Assigned(OnSoundRequest) then
      OnSoundRequest(Self, '', 0, True);
    SameName := CompareText(OldFile, S) = 0;
    if not SameName then
    begin
      if Assigned(OnViewerClear) then
        for I := 0 to CurbrFrameSet.Viewers.Count - 1 do
          OnViewerClear(CurbrFrameSet.Viewers[I]);
      OldFrameSet := CurbrFrameSet;
      FCurFrameSet := TbrFrameSet.Create(Self);
      CurFrameSet.Align := alClient;
      CurFrameSet.visible := False;
      InsertControl(CurbrFrameSet);
      CurFrameSet.SendToBack;
      CurFrameSet.Visible := True;

      try
        S1 := '';
        if Assigned(FOnGetPostRequestEx) then
          FOnGetPostRequestEx(Self, IsGet, S, Query, EncType, Referer, Reload, S1, StreamType, Stream)
        else
          FOnGetPostRequest(Self, IsGet, S, Query, Reload, S1, StreamType, Stream);
        if not Assigned(Stream) then
          raise(EfvLoadError.Create('Can''t load: ' + S));
        if S1 <> '' then
          S := S1;

        if Pos(':', S) <> 0 then
          CurbrFrameSet.URLBase := URLSubs.GetBase(S)
        else
        begin
          CurbrFrameSet.URLBase := OldFrameSet.URLBase;
          S := Combine(CurbrFrameSet.URLBase, S);
        end;

        (CurbrFrameSet as TbrFrameSet).LoadFromBrzFile(Stream, StreamType, S, Dest);
      except
        RemoveControl(CurbrFrameSet);
        CurbrFrameSet.Free;
        FCurFrameSet := OldFrameSet;
        raise;
      end;

      OldPos := 0;
      if (OldFrameSet.Viewers.Count = 1) then
      begin
        Tmp := OldFrameSet.Viewers[0];
        if Tmp is ThtmlViewer then
          OldPos := ThtmlViewer(Tmp).Position;
      end;
      OldFrameSet.UnloadFiles;
      CurbrFrameSet.Visible := True;
      if Visible then
      begin
        SendMessage(Handle, wm_SetRedraw, 0, 0);
        try
          CurbrFrameSet.BringToFront;
        finally
          SendMessage(Handle, wm_SetRedraw, 1, 0);
          Repaint;
        end;
        CurbrFrameSet.Repaint;
      end;

      RemoveControl(OldFrameSet);
      BumpHistory(OldFrameSet, OldPos);
    end
    else
    begin {Same name}
      OldPos := 0;
      if (CurbrFrameSet.Viewers.Count = 1) then
      begin
        Tmp := CurbrFrameSet.Viewers[0];
        if Tmp is ThtmlViewer then
          OldPos := ThtmlViewer(Tmp).Position;
      end;
      if Assigned(FOnGetPostRequestEx) then
        FOnGetPostRequestEx(Self, IsGet, S, Query, EncType, Referer, Reload, S1, StreamType, Stream)
      else
        FOnGetPostRequest(Self, IsGet, S, Query, Reload, S1, StreamType, Stream);
      if not Assigned(Stream) then
        raise(EfvLoadError.Create('Can''t locate cache file: ' + S));

      if S1 <> '' then
      begin
        S := S1;
        if Pos(':', S) <> 0 then
          CurbrFrameSet.URLBase := URLSubs.GetBase(S);
      end;

      (CurbrFrameSet as TbrFrameSet).LoadFromBrzFile(Stream, StreamType, S, Dest);
      BumpHistory2(OldPos); {not executed if exception occurs}
    end;
    AddVisitedLink(URL);
  finally
    EndProcessing;
  end;
end;

{----------------TFrameBrowser.HotSpotClickHandled:}

function TFrameBrowser.HotSpotClickHandled(const FullUrl: string): boolean;
var
  Handled: boolean;
begin
  Handled := False;
  if Assigned(OnHotSpotTargetClick) then
    OnHotSpotTargetClick(Self, FTarget, FullUrl, Handled);
  Result := Handled;
end;

{----------------TFrameBrowser.HotSpotClick}

procedure TFrameBrowser.HotSpotClick(Sender: TObject; const AnURL: string; var Handled: boolean);
var
  I: integer;
  Viewer: ThtmlViewer;
  FrameTarget: TFrameBase;
  S, Dest, FullUrl: string;
begin
  if Processing then
  begin
    Handled := True;
    Exit;
  end;
  Viewer := (Sender as ThtmlViewer);
  FURL := AnURL;
  FTarget := GetActiveTarget;
  FLinkAttributes.Text := Viewer.LinkAttributes.Text;
  FLinkText := Viewer.LinkText;

  SplitUrl(AnUrl, S, Dest);
  S := ConvDosToHTML(S);
  if S = '' then
    FullUrl := (Viewer.FrameOwner as TbrFrame).Source
  else if IsFullURL(S) then
    FullUrl := S
  else if Viewer.Base <> '' then
    FullUrl := Combine(UrlSubs.GetBase(ConvDosToHTML(Viewer.Base)), S)
  else
    FullUrl := Combine((Viewer.FrameOwner as TbrFrame).URLBase, S);

  Handled := HotSpotClickHandled(FullUrl + Dest);
  if not Handled then
  begin
    Handled := True;
    if (FTarget = '') or (CompareText(FTarget, '_self') = 0) then {no target or _self target}
    begin
      FrameTarget := Viewer.FrameOwner as TbrFrame;
      if not Assigned(FrameTarget) then
        Exit;
    end
    else if CurbrFrameSet.FrameNames.Find(FTarget, I) then
      FrameTarget := (CurbrFrameSet.FrameNames.Objects[I] as TbrFrame)
    else if CompareText(FTarget, '_top') = 0 then
      FrameTarget := CurbrFrameSet
    else if CompareText(FTarget, '_parent') = 0 then
    begin
      FrameTarget := (Viewer.FrameOwner as TbrFrame).Owner as TFrameBase;
      while Assigned(FrameTarget) and not (FrameTarget is TbrFrame)
        and not (FrameTarget is TbrFrameSet) do
        FrameTarget := FrameTarget.Owner as TFrameBase;
    end
    else
    begin
      if Assigned(OnBlankWindowRequest) then
      begin
        AddVisitedLink(FullUrl + Dest);
        CheckVisitedLinks;
        OnBlankWindowRequest(Self, FTarget, FullUrl + Dest);
        Handled := True;
      end
      else
        Handled := FTarget <> ''; {true if can't find target window}
      Exit;
    end;

    BeginProcessing;
    if (FrameTarget is TbrFrame) and (CurbrFrameSet.Viewers.Count = 1) and (S <> '')
      and (CompareText(S, CurbrFrameSet.FCurrentFile) <> 0) then
      FrameTarget := CurbrFrameSet; {force a new FrameSet on name change}
    try
      if FrameTarget is TbrFrame then
        TbrFrame(FrameTarget).frLoadFromBrzFile(FullUrl, Dest, '', '', Viewer.CurrentFile, True, True, False)
      else if FrameTarget is TbrFrameSet then
        Self.LoadURLInternal(FullUrl + Dest, '', '', Viewer.CurrentFile, True, False);
      CheckVisitedLinks;
    finally
      EndProcessing;
    end;
  end;
end;

{----------------TFrameBrowser.HotSpotCovered}

procedure TFrameBrowser.HotSpotCovered(Sender: TObject; const SRC: string);
var
  S, Dest, FullUrl: string;
  Viewer: ThtmlViewer;
begin
  if Assigned(OnHotSpotTargetCovered) then
  begin
    Viewer := Sender as ThtmlViewer;
    SplitUrl(SRC, S, Dest);
    S := ConvDosToHTML(S); {convert DOS names}
    if IsFullURL(S) or (Src = '') then
      FullUrl := S
    else
    begin
      if Viewer.Base <> '' then
        FullUrl := Combine(UrlSubs.GetBase(ConvDosToHTML(Viewer.Base)), S)
      else
        FullUrl := Combine((Viewer.FrameOwner as TbrFrame).URLBase, S);
    end;
    FLinkText := Viewer.LinkText;
    FLinkAttributes.Text := Viewer.LinkAttributes.Text;
    OnHotSpotTargetCovered(Sender, (Sender as ThtmlViewer).Target, FullUrl + Dest);
  end;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TFrameBrowser.CurbrFrameSet: TbrFrameSet;
begin
  Result := TbrFrameSet(CurFrameSet);
end;

{----------------TFrameBrowser.DoFormSubmitEvent}

procedure TFrameBrowser.DoFormSubmitEvent(Sender: TObject; const Action, Target, EncType,
  Method: string; Results: TStringList);
var
  S, Dest, Query: string;
  FrameTarget: TFrameBase;
  I: integer;
  Viewer: ThtmlViewer;
  UserHandled, IsGet: boolean;

  function AssembleQuery: string;
  var
    S1: string;
    I, J: integer;

    function Encode(const S: string): string;
    var
      Ch: char;
      I: integer;
    begin {convert odd chars into %xx -- does not handle the '=' sign yet}
      Result := '';
      for I := 1 to Length(S) do
      begin
        Ch := S[I];
        if Ch = ' ' then
          Result := Result + '+'
        else if not (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '=', '_', '-', '.', '*', '@']) then
          Result := Result + '%' + IntToHex(ord(Ch), 2)
        else
          Result := Result + Ch;
      end;
    end;

  begin
    Result := '';
    for I := 0 to Results.Count - 1 do
    begin
      if FEncodePostArgs then
      begin {form a string from the TStringList using '+' for spaces and '&' for separaters}
        S1 := Encode(Results[I]);
        J := Pos(' ', S1);
        while J > 0 do
        begin
          S1[J] := '+';
          J := Pos(' ', S1);
        end;
      end
      else
        S1 := Trim(Results[I]); {No encoding done}
      if I <> 0 then
        Result := Result + '&';
      Result := Result + S1;
    end;
    Results.Free;
  end;

begin
  if InFormSubmit then
    Exit;
  InFormSubmit := True;
  try
  {see if the application wants to handle this event}
    UserHandled := false;
    Viewer := (Sender as ThtmlViewer);
    if Assigned(FOnFormSubmit) then
      FOnFormSubmit(Self, Viewer, Action, Target, EncType, Method, Results, UserHandled);
    if not UserHandled then
    begin
      Query := AssembleQuery;

      if (Target = '') or (CompareText(Target, '_self') = 0) then {no target or _self target}
        FrameTarget := Viewer.FrameOwner as TbrFrame
      else if CurbrFrameSet.FrameNames.Find(Target, I) then
        FrameTarget := (CurbrFrameSet.FrameNames.Objects[I] as TbrFrame)
      else if CompareText(Target, '_top') = 0 then
        FrameTarget := CurbrFrameSet
      else if CompareText(Target, '_parent') = 0 then
      begin
        FrameTarget := (Viewer.FrameOwner as TbrFrame).Owner as TFrameBase;
        while Assigned(FrameTarget) and not (FrameTarget is TbrFrame)
          and not (FrameTarget is TbrFrameSet) do
          FrameTarget := FrameTarget.Owner as TFrameBase;
      end
      else
      begin
        if Assigned(OnBlankWindowRequest) then
          OnBlankWindowRequest(Self, Target, Action + '?' + Query);
        Exit;
      end;

      S := Action;
      I := Pos('#', S);
      if I >= 1 then
      begin
        Dest := System.Copy(S, I, Length(S) - I + 1); {local destination}
        S := System.Copy(S, 1, I - 1); {the file name}
      end
      else
        Dest := ''; {no local destination}

      BeginProcessing;
      try
        if (FrameTarget is TbrFrame) and (CurbrFrameSet.Viewers.Count = 1) and (S <> '')
          and (CompareText(S, CurbrFrameSet.FCurrentFile) <> 0) then
          FrameTarget := CurbrFrameSet; {force a new FrameSet on name change}
        if S = '' then
          S := (Viewer.FrameOwner as TbrFrame).Source
        else if not IsFullURL(S) then
          S := Combine((Viewer.FrameOwner as TbrFrame).URLBase, S);
        IsGet := CompareText(Method, 'get') = 0;
        if FrameTarget is TbrFrame then
          TbrFrame(FrameTarget).frLoadFromBrzFile(S, Dest, Query, EncType, Viewer.CurrentFile, True, IsGet, True)
        else if FrameTarget is TbrFrameSet then
          Self.LoadURLInternal(S + Dest, Query, EncType, Viewer.CurrentFile, IsGet, True);
      finally
        EndProcessing;
      end;
    end;
  finally
    InFormSubmit := False;
  end;
end;

procedure TFrameBrowser.DoURLRequest(Sender: TObject; const SRC: string; var Stream: TMemoryStream);
var
  NewURL: string;
  DocType: ThtmlFileType;
begin
  if Assigned(FOnGetPostRequestEx) then
    FOnGetPostRequestEx(Sender, True, SRC, '', '', '', False, NewURL, DocType, Stream)
  else if Assigned(FOnGetPostRequest) then
    FOnGetPostRequest(Sender, True, SRC, '', False, NewURL, DocType, Stream);
end;

{----------------TFrameBrowser.GetViewerUrlBase}

function TFrameBrowser.GetViewerUrlBase(Viewer: ThtmlViewer): string;
var
  Frame: TbrFrame;
begin
  try
    Frame := Viewer.FrameOwner as TbrFrame;
    Result := Frame.UrlBase;
  except
    Result := '';
  end;
end;

{----------------TFrameBrowser.CheckVisitedLinks}

procedure TFrameBrowser.CheckVisitedLinks;
var
  I, J, K: integer;
  S, S1: string;
  Viewer: ThtmlViewer;
begin
  if VisitedMaxCount = 0 then
    Exit;
  for K := 0 to CurbrFrameSet.Viewers.Count - 1 do
  begin
    Viewer := ThtmlViewer(CurbrFrameSet.Viewers[K]);
    for I := 0 to Visited.Count - 1 do
    begin
      S := Visited[I];
      for J := 0 to Viewer.LinkList.Count - 1 do
        with TFontObj(Viewer.LinkList[J]) do
        begin
          if Url <> '' then
          begin
            if IsFullURL(Url) then
              S1 := Url
            else if Url[1] = '#' then
              S1 := TbrFrame(Viewer.FrameOwner).Source + Url
            else
              S1 := Combine(TbrFrame(Viewer.FrameOwner).UrlBase, Url);
            if CompareText(S, S1) = 0 then
              Visited := True;
          end;
        end;
    end;
    Viewer.Invalidate;
  end;
end;

end.
