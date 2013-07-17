{   Component(s):
    tcyWebBrowser

    Description:
    Navigate, save and access hidden properties from html documents easily

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

unit cyWebBrowser;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses
  classes, cyCustomWebBrowser;

type
  TcyWebBrowser = class(TcyCustomWebBrowser)
  private
  protected
  public
    property BodyBorderStyle;
    property BodyHtml;
    property BodyTextual;
    property Busy;           // TwebBrowser.Busy seems not working (not set to false when document completed)
    property Charset;
    property DisplayTextSize;
    property Html;
    property Navigating;
    property SelectionText;
  published
    property AsynchMode;
    property DesignMode;
  end;

  TWebBrowser = class(TcyWebBrowser);
  // Let this line in order to overload the TWebBrowser class component in your projects
  // with TcyWebBrowser (declare "cyWebBrowser" in the uses of your unit after "SHDocVw" declaration) :
  // Your old TwebBrowser components will have the same properties/events/fonctions in your source code
  // and at run time but not visualized in the object inspector at design time.
  // Remove if you don' t want to.

implementation

end.
