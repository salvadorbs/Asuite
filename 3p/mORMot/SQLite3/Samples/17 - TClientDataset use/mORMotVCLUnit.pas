unit mORMotVCLUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Grids, DBGrids, 
  SynCommons, mORMot, mORMotVCL, DB;

type
  TForm1 = class(TForm)
    dbgrdData: TDBGrid;
    ds1: TDataSource;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fDataSet: TDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
var JSON: RawUTF8;
begin
  JSON := StringFromFile('..\..\exe\People.json');
  if JSON='' then
    JSON := StringFromFile('..\..\People.json');
  ds1.DataSet := nil;
  FreeAndNil(fDataSet);
  fDataSet := JSONToDataSet(self,JSON);
  ds1.DataSet := fDataSet;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fDataSet);
end;

end.
