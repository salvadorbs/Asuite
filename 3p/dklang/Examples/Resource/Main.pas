//**********************************************************************************************************************
//  $Id: Main.pas,v 1.4 2006/08/11 12:15:51 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  DKLang Localization Package
//  Copyright (c)DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit Main;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DKLang, StdCtrls;

type
  TfMain = class(TForm)
    bCancel: TButton;
    cbLanguage: TComboBox;
    lcMain: TDKLanguageController;
    lSampleMessage: TLabel;
    procedure cbLanguageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  fMain: TfMain;

implementation
{$R *.dfm}
uses SysUtils;

{$R LangFiles.res} 

  procedure TfMain.cbLanguageChange(Sender: TObject);
  var iIndex: Integer;
  begin
    iIndex := cbLanguage.ItemIndex;
    if iIndex<0 then iIndex := 0; // When there's no valid selection in cbLanguage we use the default language (Index=0)
    LangManager.LanguageID := LangManager.LanguageIDs[iIndex];
  end;

  procedure TfMain.FormCreate(Sender: TObject);
  var i: Integer;
  begin
     // Fill in the 'statically included' languages
    LangManager.RegisterLangResource(HInstance, 'LNG_RUSSIAN', 1049);
    LangManager.RegisterLangResource(HInstance, 'LNG_GERMAN',  1031);
     // Additionally, you can scan for language files in the app directory, uncomment the next line to do this
    //LangManager.ScanForLangFiles(WideExtractFileDir(WideParamStr(0)), '*.lng', False);
     // Fill cbLanguage with available languages
    for i := 0 to LangManager.LanguageCount-1 do cbLanguage.Items.Add(LangManager.LanguageNames[i]);
     // Index=0 always means the default language
    cbLanguage.ItemIndex := 0; 
  end;

end.
