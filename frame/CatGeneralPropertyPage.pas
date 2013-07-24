unit CatGeneralPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseGeneralPropertyPage, Vcl.StdCtrls,
  VirtualTrees;

type
  TfrmCatGeneralPropertyPage = class(TfrmBaseGeneralPropertyPage)
    grpSubItems: TGroupBox;
    vstCategoryItems: TVirtualStringTree;
    lblNote: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCatGeneralPropertyPage: TfrmCatGeneralPropertyPage;

implementation

{$R *.dfm}

end.
