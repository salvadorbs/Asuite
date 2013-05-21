unit notifications;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, forms, StdCtrls,
  Graphics, UxTheme, ImgList, Math, Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  THackCanvas = class(TCanvas);
  TNotificationStates = (nsFadingIn, nsVisible, nsFadingOut);

  TfrmNotification = class(TForm)
    lblTitle: TLabel;
    lblText: TLabel;
    ImageList1: TImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FState: TNotificationStates;
    FDisplayOnEndTitle: string;
    FDisplayOnEndStream: string;

    class function OtherWindowIsFullscreen: Boolean;
  protected
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMMouseActivate(var Message: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
    procedure DoShow; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    class procedure Execute(Title, Text: string);
    class procedure Stop;

    procedure Display(Title, Text: string);
    procedure StopDisplay;
  end;

var
  TheForm: TfrmNotification;

implementation

{$R *.dfm}
{ TfrmNotification }

class procedure TfrmNotification.Execute(Title, Text: string);
begin
  if OtherWindowIsFullscreen then
    Exit;

  if TheForm = nil then
  begin
    TheForm := TfrmNotification.Create(nil);
    TheForm.Display(Title, Text);
  end
  else
  begin
    TheForm.Display(Title, Text);
  end;
end;

constructor TfrmNotification.Create(AOwner: TComponent);
begin
  inherited;

  FState := nsFadingIn;
  Parent := nil;
  ClientHeight := 55;
end;

procedure TfrmNotification.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := 0;
  Params.Style := WS_POPUP or WS_THICKFRAME or WS_EX_TOPMOST;
  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE;
end;

procedure TfrmNotification.Display(Title, Text: string);
var
  TextWidth: Integer;
  function TruncateText(Text: string; MaxWidth: Integer; Font: TFont): string;
  var
    w: Integer;
    Canvas: THackCanvas;
    Size2: TSize;
  begin
    Canvas := THackCanvas.Create;
    try
      Canvas.Handle := GetDC(GetDesktopWindow);
      SelectObject(Canvas.Handle, Font.Handle);

      if MaxWidth > -1 then
      begin
        GetTextExtentPoint32(Canvas.Handle, PChar(Text), Length(Text), Size2);
        w := Size2.cx;
        if w > MaxWidth then
        begin
          SetLength(Text, Length(Text) - 1);
          Text := Text + '...';
        end;

        GetTextExtentPoint32(Canvas.Handle, PChar(Text), Length(Text), Size2);
        w := Size2.cx;
        while w > MaxWidth do
        begin
          Text := Copy(Text, 1, Length(Text) - 4) + '...';
          if Length(Text) = 3 then
          begin
            Text := '';
            Break;
          end;
          GetTextExtentPoint32(Canvas.Handle, PChar(Text), Length(Text), Size2);
          w := Size2.cx;
        end;
      end;
      Result := Text;

      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    finally
      Canvas.Free;
    end;
  end;
  function GetTextSize(Text: string; Font: TFont): TSize;
  var
    Canvas: THackCanvas;
    Size2: TSize;
  begin
    Result.cx := 0;
    Result.cy := 0;
    Canvas := THackCanvas.Create;
    try
      Canvas.Handle := GetDC(GetDesktopWindow);
      SelectObject(Canvas.Handle, Font.Handle);
      GetTextExtentPoint32(Canvas.Handle, PChar(Text), Length(Text), Size2);
      Result := Size2;
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    finally
      Canvas.Free;
    end;
  end;

begin
  case FState of
    nsFadingIn:
      begin
        TextWidth := GetTextSize(Title, lblTitle.Font).cx;
        if TextWidth > 350 then
          TextWidth := 350;
        ClientWidth := lblTitle.Left * 2 + 52 + Max(200, TextWidth);

        Left := Screen.PrimaryMonitor.WorkareaRect.Right - ClientWidth -
          GlassFrame.Right * 2 - 15;
        Top := Screen.PrimaryMonitor.WorkareaRect.Bottom - ClientHeight -
          GlassFrame.Top * 2 - 15;

        DoShow;
        lblTitle.Caption := TruncateText(Title, lblTitle.Width, lblTitle.Font);
        if Text <> '' then
          lblText.Caption := TruncateText(Text,lblText.Width, lblText.Font)
        else
          lblText.Caption := '';
  
        ShowWindow(Handle, SW_HIDE);
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
      end;
    nsVisible, nsFadingOut:
      begin
        FDisplayOnEndTitle := Title;
        FDisplayOnEndStream := Text;
        KillTimer(Handle, 0);
        KillTimer(Handle, 1);
        KillTimer(Handle, 2);
        SetTimer(Handle, 2, 20, nil);
      end;
  end;
end;

procedure TfrmNotification.DoShow;
begin
  AlphaBlend := True;
  AlphaBlendValue := 0;
  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  SetTimer(Handle, 0, 20, nil);
  SetTimer(Handle, 10, 50, nil);
end;

procedure TfrmNotification.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  KillTimer(Handle, 10);
  Action := caFree;
  TheForm := nil;
end;

class function TfrmNotification.OtherWindowIsFullscreen: Boolean;
  function RectMatches(R: TRect; R2: TRect): Boolean;
  begin
    Result := (R.Left = R2.Left) and (R.Top = R2.Top) and (R.Right = R2.Right)
      and (R.Bottom = R2.Bottom);
  end;

type
  TGetShellWindow = function(): HWND; stdcall;
var
  i: Integer;
  H, Handle: Cardinal;
  R: TRect;
  GetShellWindow: TGetShellWindow;
begin
  H := GetForegroundWindow;

  @GetShellWindow := nil;
  Handle := GetModuleHandle('user32.dll');
  if (Handle > 0) then
  begin
    @GetShellWindow := GetProcAddress(Handle, 'GetShellWindow');
  end;

  if ((H <> GetDesktopWindow) and ((@GetShellWindow <> nil) and
    (H <> GetShellWindow))) then
  begin
    GetWindowRect(H, R);
    for i := 0 to Screen.MonitorCount - 1 do
      if RectMatches(Screen.Monitors[i].BoundsRect, R) then
      begin
        Exit(True);
      end;
  end;

  Exit(False);
end;

procedure TfrmNotification.Paint;
begin
  inherited;
  ImageList1.Draw(Canvas, ClientWidth - 52, 4, 0, True);
end;

class procedure TfrmNotification.Stop;
begin
  if TheForm <> nil then
    TheForm.StopDisplay;
  TheForm.Free;
end;

procedure TfrmNotification.StopDisplay;
begin
  FDisplayOnEndTitle := '';
  FDisplayOnEndStream := '';
  KillTimer(Handle, 0);
  KillTimer(Handle, 1);
  KillTimer(Handle, 2);
  SetTimer(Handle, 2, 5, nil);
end;

procedure TfrmNotification.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  Message.Result := MA_NOACTIVATEANDEAT;
end;

procedure TfrmNotification.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTCLIENT;
end;

procedure TfrmNotification.WMTimer(var Message: TWMTimer);
begin
  if Message.TimerID = 0 then
  begin
    if AlphaBlendValue + 20 < 225 then
      AlphaBlendValue := AlphaBlendValue + 20
    else
    begin
      AlphaBlendValue := 225;
      KillTimer(Handle, 0);
      SetTimer(Handle, 1, 4000, nil);
      FState := nsVisible;
    end;
  end
  else if Message.TimerID = 1 then
  begin
    KillTimer(Handle, 1);
    SetTimer(Handle, 2, 20, nil);
    FState := nsFadingOut;
  end
  else if Message.TimerID = 2 then
  begin
    if AlphaBlendValue - 20 > 0 then
      AlphaBlendValue := AlphaBlendValue - 20
    else
    begin
      AlphaBlendValue := 0;
      KillTimer(Handle, 2);

      if FDisplayOnEndTitle <> '' then
      begin
        FState := nsFadingIn;
        Display(FDisplayOnEndTitle, FDisplayOnEndStream);
        FDisplayOnEndTitle := '';
        FDisplayOnEndStream := '';
      end
      else
      begin
        Close;
      end;
    end;
  end;

  if Message.TimerID = 10 then
  begin
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
      SWP_NOACTIVATE);
  end;
end;

end.
