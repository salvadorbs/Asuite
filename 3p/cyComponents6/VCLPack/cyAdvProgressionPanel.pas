{   Component(s):
    TcyAdvProgressionPanel

    Description:
    Run-time threaded Progression panel component

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

unit cyAdvProgressionPanel;

interface

uses Classes, Windows, Controls, SysUtils, cyCustomProgressionPanel;

type
  TcyAdvProgressionPanel = class;

  TProgressionThreadHandler = class(TThread)
  private
    FcyAdvProgressionPanel: TcyAdvProgressionPanel;
    FRunningExecute: Boolean;
    procedure UpdateGauge;
  protected
  public
    procedure Execute; override;
  end;

  TcyAdvProgressionPanel = class(TcyCustomProgressionPanel)
  private
    FProgressionThreadHandler: TProgressionThreadHandler;
    FGaugeStepInterval: Cardinal;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(ParentControl: TWinControl); override;
    procedure Close; override;
    property State;
  published
    property Alignment;
    property Autosize;
    property BorderWidth;
//    property ButtonCancel;
//    property ButtonCancelCaption;
    property Caption;
    property Degrade;
    property FlashWindow;
    property Font;
    property Height;
    property Glyph;
    property GlyphAlign;
    property Layout;
//    property Gauge;
    property GaugeMax;
    property GaugeMin;
    property Width;
    property OnOpen;
    property OnClose;
//    property OnCancelButtonClick;
    property GaugeStepInterval: Cardinal read FGaugeStepInterval write FGaugeStepInterval default 200;
  end;
  
implementation

procedure TProgressionThreadHandler.UpdateGauge;
begin
  with FcyAdvProgressionPanel.GetGauge do
  begin
    Canvas.Lock;

    if Position = Max
    then Position := Min
    else StepIt;

    Canvas.Unlock;
  end;
end;

procedure TProgressionThreadHandler.Execute;
begin
  FRunningExecute := true;

  try
    while not Terminated do
    begin
      // Synchronize(UpdateGauge);  // Don' t work because of blocking code in application' s main thread ...
      UpdateGauge;

      // Process messages for panel :
// Doesn' t work ...
//      if FcyAdvProgressionPanel.GetCancelButton.Visible
//      then FcyAdvProgressionPanel.ProcessMessagesFromPanel;

      sleep(FcyAdvProgressionPanel.GaugeStepInterval);
    end;
  except

  end;
  
  // SetMessageQueue(0);
  FRunningExecute := false;
end;

{TcyAdvProgressionPanel}
constructor TcyAdvProgressionPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGaugeStepInterval := 200;
end;

procedure TcyAdvProgressionPanel.Open(ParentControl: TWinControl);
begin
  if State = psOpened then EXIT;

  // Create components before creating thread :
  Inherited;

  FProgressionThreadHandler := TProgressionThreadHandler.Create(true);
  FProgressionThreadHandler.FcyAdvProgressionPanel := Self;
  FProgressionThreadHandler.FRunningExecute := false;
  FProgressionThreadHandler.FreeOnTerminate := false;
  FProgressionThreadHandler.Suspended := false;
end;

procedure TcyAdvProgressionPanel.Close;
begin
  if State <> psOpened then EXIT;

  // Terminated thread before destroy components :
  FProgressionThreadHandler.Terminate;

  // Wait while code exit thread execute code :
  while FProgressionThreadHandler.FRunningExecute do
    Sleep(50);    // Executed on application main thread ... 

  FProgressionThreadHandler.Free;
  Inherited;
end;

end.
