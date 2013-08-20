//**********************************************************************************************************************
//  $Id: DKLang_MDI_Demo.dpr,v 1.3 2006-06-17 04:19:28 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  DKLang Localization Package
//  Copyright 2002-2006 DK Software, http://www.dk-soft.org
//**********************************************************************************************************************
program DKLang_MDI_Demo;

uses
  Forms,
  Main in 'Main.pas' {fMain},
  ufMDIChild in 'ufMDIChild.pas' {fMDIChild};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
