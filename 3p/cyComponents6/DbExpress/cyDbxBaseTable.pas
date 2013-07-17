unit cyDbxBaseTable;

{$I ..\Core\cyCompilerDefines.inc}

{   Component(s):
    tcyDbxTable

    Description:
    tcyDbxTable allows:
    - automatic server side updates in order to simplify SQL table records handling.
    - automatic refresh records from server after delete/post a record.

    The purpose is to reproduce BDE TTable component behavior: no worry with server side updates/refresh.
    Because tcyDbxTable descends of TClientDataset, if you set CachedUpdates to true, tcyDbxTable behavior is equal to TClientDataset.

    *** Important notes of differences between TTable and MySQL handling ***
    - AutoInc fields handling: In both tcyDbxTable and associated provider's query, put "required" property to false
    - For fast updates and avoid reconcile errors, put TDatasetProvider.UpdateMode to upWhereKeyOnly.
      Don' t forget to add [pfInKey] in ProviderFlags property to associated provider's query unique index field.


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

interface

uses Classes, Dialogs, DB, DBClient, SqlExpr, Provider, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} cyDBX, cyDbxReconcileError;

type
  TRefreshType = (rfNone, rtTable, rtRecord);

  TcyDbxAutoRefreshOptions = class(TPersistent)
  private
    FAfterPostNewRecord: TRefreshType;
    FMovingToFirst: Boolean;
    FAfterDelete: Boolean;
    FMovingToLast: Boolean;
    FAfterPostModifiedRecord: TRefreshType;
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property AfterDelete: Boolean read FAfterDelete write FAfterDelete default true;
    property MovingToFirst: Boolean read FMovingToFirst write FMovingToFirst default true;
    property MovingToLast: Boolean read FMovingToLast write FMovingToLast default true;
    property AfterPostModifiedRecord: TRefreshType read FAfterPostModifiedRecord write FAfterPostModifiedRecord default rtTable;
    property AfterPostNewRecord: TRefreshType read FAfterPostNewRecord write FAfterPostNewRecord default rtTable;
  end;

  TcyDbxBaseTableUpdatesOptions = class(TPersistent)
  private
    FOwner: TCustomClientDataSet;
    FMaxErrors: Integer;
    FSendResetAutoIncCmd: Boolean;
  protected
    property MaxErrors: Integer read FMaxErrors write FMaxErrors default -1;
    property SendResetAutoIncCmd: Boolean read FSendResetAutoIncCmd write FSendResetAutoIncCmd default false;
  public
    constructor Create(AOwner: TComponent); virtual;
  published
  end;

  TcyDbxRecordLockOptions = class(TPersistent)
  private
    FOwner: TCustomClientDataSet;
    FEnabled: Boolean;
    FWaitSeconds: Cardinal;
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property WaitSeconds: Cardinal read FWaitSeconds write FWaitSeconds default 0;
  end;

implementation

{ TcyDbxAutoRefreshOptions }
constructor TcyDbxAutoRefreshOptions.Create(AOwner: TComponent);
begin
  FAfterDelete := true;
  FMovingToFirst := true;
  FMovingToLast := true;
  FAfterPostModifiedRecord := rtTable;
  FAfterPostNewRecord := rtTable;
end;


{ TcyDbxBaseTableUpdatesOptions }
constructor TcyDbxBaseTableUpdatesOptions.Create(AOwner: TComponent);
begin
  FMaxErrors := -1;
  FSendResetAutoIncCmd := false;
  FOwner := TCustomClientDataSet(AOwner);
end;

{ TcyDbxRecordLockOptions }
constructor TcyDbxRecordLockOptions.Create(AOwner: TComponent);
begin
  FOwner := TCustomClientDataSet(AOwner);
  FEnabled := false;
  FWaitSeconds := 0;
end;

end.
