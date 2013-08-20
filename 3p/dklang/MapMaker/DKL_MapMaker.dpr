///*********************************************************************************************************************
///  $Id: DKL_MapMaker.dpr,v 1.2 2006-06-17 04:19:28 dale Exp $
///---------------------------------------------------------------------------------------------------------------------
///  DKLang Localization Package
///  Copyright 2002-2006 DK Software, http://www.dk-soft.org
///*********************************************************************************************************************
///
/// The contents of this package are subject to the Mozilla Public License
/// Version 1.1 (the "License"); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
///
/// Alternatively, you may redistribute this library, use and/or modify it under the
/// terms of the GNU Lesser General Public License as published by the Free Software
/// Foundation; either version 2.1 of the License, or (at your option) any later
/// version. You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/
///
/// Software distributed under the License is distributed on an "AS IS" basis,
/// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
/// specific language governing rights and limitations under the License.
///
/// The initial developer of the original code is Dmitry Kann, http://www.dk-soft.org/
///
///**********************************************************************************************************************
program DKL_MapMaker;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  DKL_MapMakerTypes in 'DKL_MapMakerTypes.pas';

begin
  if ParamCount<1 then begin
    Writeln('Usage: DKL_MapMaker <executable_file_name>');
    Halt(1);
  end else if DKLang_MakeResMap(PChar(ParamStr(1))) then
    Writeln(ParamStr(1)+' processed OK.')
  else begin
    Writeln('Error processing '+ParamStr(1));
    Halt(1);
  end;
end.
