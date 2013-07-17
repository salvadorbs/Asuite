{   Component(s):
    tcyRunTimeResize

    Description:
    Allow simple moving and resizing single component.

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

unit cyRunTimeResize;

interface

uses VCL.cyTypes, VCL.cyClasses, Classes, Controls;

type
  TcyRunTimeResize = class(TComponent)
  private
    FOptions: TcyRunTimeDesign;
    procedure SetOptions(const Value: TcyRunTimeDesign);
    procedure SetControl(const Value: TControl);
    function GetControl: TControl;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartJob(X, Y: Integer);
    procedure DoJob(X, Y: Integer);
    procedure EndJob(X, Y: Integer);
  published
    property Control: TControl read GetControl write SetControl;
    property Options: TcyRunTimeDesign read FOptions write SetOptions;
  end;


implementation

{ TcyRunTimeResize }
constructor TcyRunTimeResize.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TcyRunTimeDesign.Create(Self);
end;

destructor TcyRunTimeResize.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure TcyRunTimeResize.StartJob(X, Y: Integer);
begin
  FOptions.StartJob(X, Y);
end;

procedure TcyRunTimeResize.DoJob(X, Y: Integer);
begin
  FOptions.DoJob(X, Y);
end;

procedure TcyRunTimeResize.EndJob(X, Y: Integer);
begin
  FOptions.EndJob(X, Y);
end;

function TcyRunTimeResize.GetControl: TControl;
begin
  RESULT := FOptions.Control;
end;

procedure TcyRunTimeResize.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Assigned(FOptions) then
    if FOptions.Control <> nil then
      if (Operation = opRemove) and (AComponent = FOptions.Control) then
        FOptions.Control := nil;
end;

procedure TcyRunTimeResize.SetControl(const Value: TControl);
begin
  if FOptions.Control <> Value then
  begin
    FOptions.Job := rjNothing;
    FOptions.Control := Value;

    if Value <> nil then
      Value.FreeNotification(Self);  // Inform TcyRunTimeResize if component removed ...
  end;
end;

procedure TcyRunTimeResize.SetOptions(const Value: TcyRunTimeDesign);
begin
  FOptions := Value;
end;

end.
