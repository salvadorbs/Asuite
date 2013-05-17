{   Component(s):
    tcyAttract

    Description:
    Attract users attention into one visual control like a button with an animation around the control.

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

unit cyAttract;

interface

uses Classes, Windows, Types, Graphics, Controls, VCL.cyGraphics;

const
  constInterval = 3000;  // Interval in milliseconds between 2 animations ...

type
  TcyAttract = class;

  TAttractFrame = class(TCollectionItem)
  private
    FPen: TPen;
    FTimeStart: cardinal;
    FSizePercent: Cardinal;
    FTimeEnd: cardinal;
  protected
    procedure DrawAttractFrame;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Pen: TPen read FPen write FPen;
    property SizePercent: Cardinal read FSizePercent write FSizePercent;
    property TimeStart: cardinal read FTimeStart write FTimeStart;
    property TimeEnd: cardinal read FTimeEnd write FTimeEnd;
  end;

  TAttractFrameClass = class of TAttractFrame;

  TAttractFrames = Class(TCollection)
  private
    FAttract: TcyAttract;
    function GeTAttractFrame(Index: Integer): TAttractFrame;
  protected
    function GetOwner: TPersistent; Override;
  public
    constructor Create(aControl: TComponent; FrameClass: TAttractFrameClass);
    function Add: TAttractFrame;
    property Items[Index: Integer]: TAttractFrame read GeTAttractFrame; default;
  end;

  TAnimationThreadHandler = class(TThread)
  private
    FAttract: TcyAttract;
    FRunningExecute: Boolean;
  protected
  public
    procedure Execute; override;
  end;

  TcyAttract = class(TComponent)
  private
    FAnimationThreadHandler: TAnimationThreadHandler;
    FEnabled: boolean;
    FInterval: Cardinal;
    FFluid: boolean;
    FFrames: TAttractFrames;
    FControl: TControl;
    procedure SetFrames(const Value: TAttractFrames);
    procedure SetEnabled(const Value: boolean);
  protected
    FBrush: TBrush;
    FPrevBrush: HBrush;
    FAttractFrameDC: HDC;
    FDrawMotionControl: TWinControl;
    procedure AllocateAttractFrameDC;
    procedure ReleaseAttractFrameDC;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Control: TControl read FControl write FControl;
    property Enabled: boolean read FEnabled write SetEnabled default false;
    property Interval: Cardinal read FInterval write FInterval default constInterval;
    property Fluid: boolean read FFluid write FFluid default false;
    property Frames: TAttractFrames read FFrames write SetFrames;
  end;

implementation

{ TAttractFrame }
constructor TAttractFrame.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSizePercent := 150;
  FTimeStart := 0;
  FTimeEnd := 100;
  FPen := TPen.Create;
  FPen.Color := clBlack;
  FPen.Width := 1;
end;

destructor TAttractFrame.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TAttractFrame.DrawAttractFrame;
var
  R: TRect;
  AttractControl: TcyAttract;
begin
  AttractControl := TAttractFrames(Collection).FAttract;

  // Calculate Rect:
  R := AttractControl.FControl.ClientRect;
  InflateRectPercent(R, (FSizePercent-100)/100);

  // Get control Rect on the screen:
  R := ClientToScreenRect(AttractControl.FControl, R);

  // Get Control Rect on FDrawMotionControl :
  R := ScreenToClientRect(AttractControl.FDrawMotionControl, R);

  // Draw frame :

  // Top line:
  PatBlt(AttractControl.FAttractFrameDC, R.Left, R.Top, R.Right-R.Left, FPen.Width, PATINVERT);

  // Right line:
  PatBlt(AttractControl.FAttractFrameDC, R.Right-FPen.Width, R.Top + FPen.Width, FPen.Width, R.Bottom-R.Top - FPen.Width*2, PATINVERT);

  // Bottom line:
  PatBlt(AttractControl.FAttractFrameDC, R.Left, R.Bottom - FPen.Width, R.Right-R.Left, FPen.Width, PATINVERT);

  // Left line:
  PatBlt(AttractControl.FAttractFrameDC, R.Left, R.Top + FPen.Width, FPen.Width, R.Bottom-R.Top - FPen.Width*2, PATINVERT);


//  AttractControl.FBrush.Color := clBlue;
//  Windows.FrameRect(AttractControl.FAttractFrameDC, R, SelectObject(AttractControl.FAttractFrameDC, AttractControl.FBrush.Handle));
end;

{ TAttractFrames }
constructor TAttractFrames.Create(aControl: TComponent; FrameClass: TAttractFrameClass);
begin
  inherited Create(FrameClass);
  FAttract := TcyAttract(aControl);
end;

function TAttractFrames.Add: TAttractFrame;
begin
  Result := TAttractFrame(inherited Add);
  Result.Changed(false);
end;

function TAttractFrames.GeTAttractFrame(Index: Integer): TAttractFrame;
begin
  Result := TAttractFrame(inherited Items[Index]);
end;

function TAttractFrames.GetOwner: TPersistent;
begin
  RESULT := FAttract;
end;

{ TAnimationThreadHandler }
procedure TAnimationThreadHandler.Execute;
var i: Integer;
begin
  FRunningExecute := true;

  try
    while not Terminated do
    begin
      for i := 0 to FAttract.FFrames.Count-1 do
        if not Terminated
        then begin
          Synchronize(FAttract.FFrames[i].DrawAttractFrame);
          sleep(100);  // Sleep in this thread ...
        end
        else
          Break;

      FAttract.FDrawMotionControl.Invalidate;

      if not Terminated
      then Sleep(FAttract.Interval);  // Sleep between 2 animations ...
    end;
  except

  end;

  FRunningExecute := false;
end;

{ TcyAttract }
constructor TcyAttract.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := false;
  FInterval := constInterval;
  FFluid := false;
  FFrames := TAttractFrames.Create(self, TAttractFrame);
end;

destructor TcyAttract.Destroy;
begin
  FFrames.Free;
  FFrames := Nil;

  inherited;
end;

procedure TcyAttract.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;

  if not (csDesigning in ComponentState)
  then
    if FEnabled
    then begin
      AllocateAttractFrameDC;

      // Create thread for animation :
      FAnimationThreadHandler := TAnimationThreadHandler.Create(true);
      FAnimationThreadHandler.FAttract := Self;
      FAnimationThreadHandler.FRunningExecute := false;
      FAnimationThreadHandler.FreeOnTerminate := false;
      FAnimationThreadHandler.Suspended := false;
    end
    else begin
      // Terminated thread before destroy components :
      FAnimationThreadHandler.Terminate;

      // Wait while code exit thread execute code :
//      while FAnimationThreadHandler.FRunningExecute do
//        Sleep(50);    // Executed on application main thread ...

      FAnimationThreadHandler.Free;
      ReleaseAttractFrameDC;
    end;
end;

procedure TcyAttract.SetFrames(const Value: TAttractFrames);
begin
  FFrames := Value;
end;

procedure TcyAttract.AllocateAttractFrameDC;
begin
  // Determine FDrawMotionControl control :
  FDrawMotionControl := FControl.Parent;
  while FDrawMotionControl.Parent <> Nil do
    FDrawMotionControl := FDrawMotionControl.Parent;

  FAttractFrameDC := GetDCEx(FDrawMotionControl.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);

  if FBrush = nil then
  begin
    FBrush := TBrush.Create;
{$IF NOT DEFINED(LINUX)}
    FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
{$ELSE}
    FBrush.Color := clGray;
{$IFEND}
  end;
  FPrevBrush := SelectObject(FAttractFrameDC, FBrush.Handle);
end;

procedure TcyAttract.ReleaseAttractFrameDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FAttractFrameDC, FPrevBrush);
  ReleaseDC(FDrawMotionControl.Handle, FAttractFrameDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

end.
