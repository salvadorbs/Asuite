{   * ***** BEGIN LICENSE BLOCK *****
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

unit cyformAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls;

type
  TFrmAbout = class(TForm)
    Image1: TImage;
    LblPackageName: TLabel;
    LblVersion: TLabel;
    LblAutor: TLabel;
    EWebSite: TEdit;
    EMail: TEdit;
    LblLocalization: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmAbout: TFrmAbout;

  procedure CindyDesignAboutForm;

implementation

uses cyDsnResource;

{$R *.dfm}

procedure CindyDesignAboutForm;
begin
  FrmAbout := TFrmAbout.Create(Nil);
  FrmAbout.LblPackageName.Caption := RSPackName;
  FrmAbout.LblVersion.Caption := 'Version ' + CindyComponentsVersion;
  FrmAbout.LblAutor.Caption := 'Autor: ' + RSAutor;
  FrmAbout.LblLocalization.Caption := 'Localization: ' + RSLocalization;
  FrmAbout.EWebSite.Text := RSWebSite;
  FrmAbout.EMail.Text := RSEMAIL;
  FrmAbout.ShowModal;
  FrmAbout.Free;
end;

end.
