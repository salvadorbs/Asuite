{   Component(s):
    tcyCustomGauge

    Description:
    Base for measure components

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

unit cyCustomMeasure;

interface

uses classes, Windows, Graphics, VCL.cyTypes, VCL.cyClasses, cyBaseMeasure;

type
  TcyCustomMeasure = class(TcyBaseMeasure)
  private
    FBevels: TcyBevels;
    procedure SetBevels(const Value: TcyBevels);
  protected
    property Bevels: TcyBevels read FBevels write SetBevels;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

{ TcyCustomMeasure }
constructor TcyCustomMeasure.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBevels := TcyBevels.Create(self, TcyBevel);

  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState
  then
    if Owner <> nil
    then
      if not (csLoading in Owner.ComponentState)  // we have just added the component at design time
      then begin
        with FBevels.Add do    // Frame
        begin
          HighlightColor := clBlack;
          ShadowColor := clBlack;
        end;

        with FBevels.Add do    // Inner 3D frame
          Width := 3;

        with FBevels.Add do    // Contrast Frame
          Style := bcLowered;

        with FBevels.Add do    // Border between Bevels and Shape
        begin
          HighlightColor := clBlack;
          ShadowColor := clBlack;
          Width := 1;
        end;
      end;
end;

destructor TcyCustomMeasure.Destroy;
begin
  FBevels.Free;
  FBevels := Nil;
  inherited Destroy;
end;

procedure TcyCustomMeasure.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

end.
