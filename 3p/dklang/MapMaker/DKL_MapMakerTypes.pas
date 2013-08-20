///*********************************************************************************************************************
///  $Id: DKL_MapMakerTypes.pas,v 1.2 2006-06-17 04:19:28 dale Exp $
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
unit DKL_MapMakerTypes;

interface

const
  SDKLang_ResMapMakerDLL = 'dklmapmk.dll';

  function  DKLang_MakeResMap(pcExeName: PAnsiChar): LongBool; stdcall;

implementation

  function DKLang_MakeResMap; external SDKLang_ResMapMakerDLL name 'MakeResMap';

end.
