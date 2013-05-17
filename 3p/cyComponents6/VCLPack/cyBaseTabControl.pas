{   Component(s):
    tcyTabControl

    Description:
    Base unit for TabControls components.

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
unit cyBaseTabControl;

interface

uses Classes, Windows, Graphics, VCL.cyClasses, VCL.cyGraphics, VCL.cyTypes, Buttons, ComCtrls, StdCtrls;

type
  TcyTabStyle = (tsTab, tsButton, tsFlatButton);
  TcyTabParameter = (tpActive, tpDrawActiveLeftFrame, tpDrawActiveRightFrame);
  TcyTabParameters = set of TcyTabParameter;

  TProcAdvancedDrawTabBackground = procedure (Sender: TObject; const Tab: Integer; var TabStyle: TcyTabStyle; const aRect: TRect; var TmpColors: TcyGradient; var Draw: Boolean) of object;
  TProcAdvancedDrawTab = procedure (Sender: TObject; const Tab: Integer; const aRect: TRect; var Draw: Boolean) of object;

  procedure DrawTabStyleBackground(OnCanvas: TCanvas; const TabStyle: TcyTabStyle; const TabPosition: TTabPosition; const aRect: TRect; const Colors: TcyGradient; const Flags: TcyTabParameters);
  procedure DrawTabStyleCaption(OnCanvas: TCanvas; aCaption: String; TabPosition: TTabPosition; const aRect: TRect);

implementation

procedure DrawTabBackground(OnCanvas: TCanvas; TabPosition: TTabPosition; aRect: TRect; Colors: TcyGradient; Flags: TcyTabParameters);
begin
  // *** Background color *** //
  Colors.Draw(OnCanvas, aRect);
  OnCanvas.Pen.Color := clGray;

  // *** Frames *** //

  // Inactive left + right frame :
  if tpActive in Flags then
  begin
    if tpDrawActiveLeftFrame in Flags then
    begin
      OnCanvas.MoveTo(aRect.Left, aRect.Bottom-1);
      OnCanvas.LineTo(aRect.Left, aRect.Top);
    end;

    if tpDrawActiveRightFrame in Flags then
    begin
      OnCanvas.MoveTo(aRect.Right-1, aRect.Bottom-1);
      OnCanvas.LineTo(aRect.Right-1, aRect.Top);
    end;

    // Top frame :
    OnCanvas.MoveTo(aRect.Left, aRect.Top);
    OnCanvas.LineTo(aRect.Right, aRect.Top);
  end
  else begin
    // Active left frame (right side of non active tab) :
    if tpDrawActiveLeftFrame in Flags then
    begin
      OnCanvas.MoveTo(aRect.Right-1, aRect.Bottom-1);
      OnCanvas.LineTo(aRect.Right-1, aRect.Top-2);

      // Clear undesired Windows TabControl points :
      OnCanvas.Pixels[aRect.Right-2, aRect.Top-2] := OnCanvas.Pixels[aRect.Right-3, aRect.Top-2];
      OnCanvas.Pixels[aRect.Right-2, aRect.Top-1] := OnCanvas.Pixels[aRect.Right-3, aRect.Top-1];
    end
    else begin
      OnCanvas.MoveTo(aRect.Right-1, aRect.Bottom-1);
      OnCanvas.LineTo(aRect.Right-1, aRect.Top);
    end;

    // Active right frame (left side of non active tab) :
    if tpDrawActiveRightFrame in Flags then
    begin
      OnCanvas.MoveTo(aRect.Left, aRect.Bottom-1);
      OnCanvas.LineTo(aRect.Left, aRect.Top-2);

      // Clear undesired Windows TabControl points :
      OnCanvas.Pixels[aRect.Left+1, aRect.Top-2] := OnCanvas.Pixels[aRect.Left+2, aRect.Top-2];
      OnCanvas.Pixels[aRect.Left+1, aRect.Top-1] := OnCanvas.Pixels[aRect.Left+2, aRect.Top-1];
    end
    else begin
      OnCanvas.MoveTo(aRect.Left, aRect.Bottom-1);
      OnCanvas.LineTo(aRect.Left, aRect.Top);
    end;

    // Top frame :
    OnCanvas.MoveTo(aRect.Left, aRect.Top);
    OnCanvas.LineTo(aRect.Right-1, aRect.Top);

    // Bottom frame :
    if TabPosition = tpTop then
    begin
      OnCanvas.MoveTo(aRect.Left, aRect.Bottom-1);
      OnCanvas.LineTo(aRect.Right-1, aRect.Bottom-1);
    end;
  end;
end;

procedure DrawFlatButtonBackground(OnCanvas: TCanvas; aRect: TRect; Colors: TcyGradient; const Flags: TcyTabParameters);
var ButtonState: TButtonState;
begin
  if tpActive in Flags
  then ButtonState := bsDown
  else ButtonState := bsUp;

  cyDrawSpeedButtonFace(OnCanvas, aRect, Colors.FromColor, Colors.ToColor, ButtonState, false, false);
end;

procedure DrawButtonBackground(OnCanvas: TCanvas; aRect: TRect; Colors: TcyGradient; const Flags: TcyTabParameters);
var ButtonState: TButtonState;
begin
  if tpActive in Flags
  then ButtonState := bsDown
  else ButtonState := bsUp;

  cyDrawButtonFace(OnCanvas, aRect, Colors.FromColor, Colors.ToColor, ButtonState, false, false);
end;

procedure DrawTabStyleBackground(OnCanvas: TCanvas; const TabStyle: TcyTabStyle; const TabPosition: TTabPosition; const aRect: TRect; const Colors: TcyGradient; const Flags: TcyTabParameters);
begin
  case TabStyle of
    tsTab:        DrawTabBackground(OnCanvas, TabPosition, aRect, Colors, Flags);
    tsFlatButton: DrawFlatButtonBackground(OnCanvas, aRect, Colors, Flags);
    tsButton:     DrawButtonBackground(OnCanvas, aRect, Colors, Flags);
  end;
end;

procedure DrawTabStyleCaption(OnCanvas: TCanvas; aCaption: String; TabPosition: TTabPosition; const aRect: TRect);
var
  DrawStyle: Longint;
  TmpFont: TFont;
  TmpRect: TRect;
begin
  OnCanvas.Brush.Style := bsClear;

  case TabPosition of
    tpTop, tpBottom:
      cyDrawSingleLineText(OnCanvas, aCaption, aRect, taCenter, tlCenter);

    tpLeft:
    begin
      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE;

      TmpFont := cyCreateFontIndirect(OnCanvas.Font, coVertical);
      try
        OnCanvas.Font.Assign(TmpFont);
        TmpRect := aRect;
        cyDrawVerticalText(OnCanvas, aCaption, TmpRect, DrawStyle, coVertical, taCenter, tlCenter);
      finally
        TmpFont.Free;
      end;
    end;

    tpRight:
    begin
      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE;

      TmpFont := cyCreateFontIndirect(OnCanvas.Font, coVerticalReversed);
      try
        OnCanvas.Font.Assign(TmpFont);
        TmpRect := aRect;
        cyDrawVerticalText(OnCanvas, aCaption, TmpRect, DrawStyle, coVerticalReversed, taCenter, tlCenter);
      finally
        TmpFont.Free;
      end;
    end;
  end;

  OnCanvas.Brush.Style := bsSolid;
end;

end.
