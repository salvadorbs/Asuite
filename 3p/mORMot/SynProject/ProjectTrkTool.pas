/// PVCS Tracker API connection
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectTrkTool;

(*
    This file is part of SynProject.

    Synopse SynProject. Copyright (C) 2008-2011 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    SynProject is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or (at
    your option) any later version.

    SynProject is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with SynProject. If not, see <http://www.gnu.org/licenses/>.

*)

interface

uses
  Windows, Classes, SysUtils;


const
  TRK_VERSION_ID = 500001;

type
  trkError  = integer;

  TRK_ACCESS_MODE             = cardinal;
  TRK_ASSOC_HANDLE            = cardinal;
  TRK_ATTFILE_HANDLE          = cardinal;
  TRK_BOOL                    = cardinal;
  TRK_CHAR                    = cardinal;
  TRK_CHOICE_OPTION           = cardinal;
  TRK_CSTR                    = PAnsiChar;
  TRK_DATE_FORMAT             = cardinal;
  TRK_DATE_OPTION             = cardinal;
  TRK_DBMS_LOGIN_MODE         = cardinal;
  TRK_EMB_QUOTE               = cardinal;
  TRK_EXPORT_HANDLE           = cardinal;
  TRK_FIELD_TYPE              = cardinal;
  TRK_FILE_STORAGE_MODE       = cardinal;
  TRK_HANDLE                  = cardinal;
  TRK_IMPORT_HANDLE           = cardinal;
  TRK_LIST_LINK_ORDER         = cardinal;
  TRK_NOTE_HANDLE             = cardinal;
  TRK_NUMBER_OPTION           = cardinal;
  TRK_RECORD_HANDLE           = cardinal;
  TRK_RECORD_TYPE             = cardinal;
  TRK_STR                     = PAnsiChar;
  TRK_TIME                    = cardinal;
  TRK_TIME_FORMAT             = cardinal;
  TRK_TRANSACTION_ID          = cardinal;
  PTRK_TRANSACTION_ID         = ^TRK_TRANSACTION_ID;
  TRK_USER_OPTION             = cardinal;
  TRK_VERSION_ID_TYPE         = cardinal;

  TRKCALLBACK = function(
      msg:            cardinal;
      param1:         cardinal;
      param2:         cardinal;
      strParam:       TRK_CSTR;
      userData:       cardinal
      ): trkError; stdcall;

  PTRKCALLBACK                = TRKCALLBACK;


const
  // Values for enum trkError
  TRK_SUCCESS                     = 0;
  TRK_E_VERSION_MISMATCH          = 1;
  TRK_E_OUT_OF_MEMORY             = 2;
  TRK_E_BAD_HANDLE                = 3;
  TRK_E_BAD_INPUT_POINTER         = 4;
  TRK_E_BAD_INPUT_VALUE           = 5;
  TRK_E_DATA_TRUNCATED            = 6;
  TRK_E_NO_MORE_DATA              = 7;
  TRK_E_LIST_NOT_INITIALIZED      = 8;
  TRK_E_END_OF_LIST               = 9;
  TRK_E_NOT_LOGGED_IN             = 10;
  TRK_E_SERVER_NOT_PREPARED       = 11;
  TRK_E_BAD_DATABASE_VERSION      = 12;
  TRK_E_UNABLE_TO_CONNECT         = 13;
  TRK_E_UNABLE_TO_DISCONNECT      = 14;
  TRK_E_UNABLE_TO_START_TIMER     = 15;
  TRK_E_NO_DATA_SOURCES           = 16;
  TRK_E_NO_PROJECTS               = 17;
  TRK_E_WRITE_FAILED              = 18;
  TRK_E_PERMISSION_DENIED         = 19;
  TRK_E_SET_FIELD_DENIED          = 20;
  TRK_E_ITEM_NOT_FOUND            = 21;
  TRK_E_CANNOT_ACCESS_DATABASE    = 22;
  TRK_E_CANNOT_ACCESS_QUERY       = 23;
  TRK_E_CANNOT_ACCESS_INTRAY      = 24;
  TRK_E_CANNOT_OPEN_FILE          = 25;
  TRK_E_INVALID_DBMS_TYPE         = 26;
  TRK_E_INVALID_RECORD_TYPE       = 27;
  TRK_E_INVALID_FIELD             = 28;
  TRK_E_INVALID_CHOICE            = 29;
  TRK_E_INVALID_USER              = 30;
  TRK_E_INVALID_SUBMITTER         = 31;
  TRK_E_INVALID_OWNER             = 32;
  TRK_E_INVALID_DATE              = 33;
  TRK_E_INVALID_STORED_QUERY      = 34;
  TRK_E_INVALID_MODE              = 35;
  TRK_E_INVALID_MESSAGE           = 36;
  TRK_E_VALUE_OUT_OF_RANGE        = 37;
  TRK_E_WRONG_FIELD_TYPE          = 38;
  TRK_E_NO_CURRENT_RECORD         = 39;
  TRK_E_NO_CURRENT_NOTE           = 40;
  TRK_E_NO_CURRENT_ATTACHED_FILE  = 41;
  TRK_E_NO_CURRENT_ASSOCIATION    = 42;
  TRK_E_NO_RECORD_BEGIN           = 43;
  TRK_E_NO_MODULE                 = 44;
  TRK_E_USER_CANCELLED            = 45;
  TRK_E_SEMAPHORE_TIMEOUT         = 46;
  TRK_E_SEMAPHORE_ERROR           = 47;
  TRK_E_INVALID_SERVER_NAME       = 48;
  TRK_E_NOT_LICENSED              = 49;
  TRK_E_RECORD_LOCKED             = 50;
  TRK_E_RECORD_NOT_LOCKED         = 51;
  TRK_E_UNMATCHED_PARENS          = 52;
  TRK_E_NO_CURRENT_TRANSITION     = 53;
  TRK_E_NO_CURRENT_RULE           = 54;
  TRK_E_UNKNOWN_RULE              = 55;
  TRK_E_RULE_ASSERTION_FAILED     = 56;
  TRK_E_ITEM_UNCHANGED            = 57;
  TRK_E_TRANSITION_NOT_ALLOWED    = 58;
  TRK_E_NO_CURRENT_STYLESHEET     = 59;
  TRK_E_NO_CURRENT_FORM           = 60;
  TRK_E_NO_CURRENT_VALUE          = 61;
  TRK_E_FORM_FIELD_ACCESS         = 62;
  TRK_E_INVALID_QBID_STRING       = 63;
  TRK_E_FORM_INVALID_FIELD        = 64;
  TRK_E_PARTIAL_SUCCESS           = 65;
  TRK_NUMBER_OF_ERROR_CODES       = TRK_E_PARTIAL_SUCCESS;

  TRKEXP_ERROR_CODE_BASE          = 10000;
  TRKEXP_E_EXPORT_WRONG_VERSION   = TRKEXP_ERROR_CODE_BASE;
  TRKEXP_E_EXPORTSET_NOT_INIT     = 10001;
  TRKEXP_E_NO_EXPSET_NAME         = 10002;
  TRKEXP_E_BAD_EXPSET_NAME        = 10003;
  TRKEXP_E_EXPSET_FAIL_CREATE     = 10004;
  TRKEXP_E_IMPORTMAP_NOT_INIT     = 10005;
  TRKEXP_E_NO_IMPMAP_NAME         = 10006;
  TRKEXP_E_BAD_IMPMAP_NAME        = 10007;
  TRKEXP_E_IMPMAP_FAIL_CREATE     = 10008;
  TRKEXP_E_IMP_VALIDATE_FAIL      = 10009;
  TRKEXP_E_USER_NOEXIST           = 10010;
  TRKEXP_E_USER_ADD               = 10011;
  TRKEXP_E_IMPORT_NOT_INIT        = 10012;
  TRKEXP_E_BAD_EMBEDDED_QUOTE_ARG = 10013;
  TRKEXP_E_BAD_DATEFORMAT_ARG     = 10014;
  TRKEXP_E_BAD_TIMEFORMAT_ARG     = 10015;
  TRKEXP_E_BAD_CHOICE_OPTION_ARG  = 10016;
  TRKEXP_E_BAD_USER_OPTION_ARG    = 10017;
  TRKEXP_E_BAD_NUMBER_OPTION_ARG  = 10018;
  TRKEXP_E_BAD_DATE_OPTION_ARG    = 10019;
  TRKEXP_E_ALL_NOTES_SELECTED     = 10020;
  TRKEXP_E_READ_EXPORTHDR         = 10021;
  TRKEXP_E_WRITE_EXPORTHDR        = 10022;
  TRKEXP_E_READ_RECORDHDR         = 10023;
  TRKEXP_E_WRITE_RECORDHDR        = 10024;
  TRKEXP_E_WRITE_FIELD            = 10025;
  TRKEXP_E_OPEN_FILE              = 10026;
  TRKEXP_E_READ_FIELD             = 10027;
  TRKEXP_E_READ_FIELD_WRONG_TYPE  = 10028;
  TRKEXP_E_BAD_ITEM_TYPE          = 10029;
  TRKEXP_E_READ_FROM_DB           = 10030;
  TRKEXP_E_WRITE_TO_DB            = 10031;
  TRKEXP_E_BAD_DATE               = 10032;
  TRKEXP_E_BAD_CHOICE             = 10033;
  TRKEXP_E_BAD_NUMBER             = 10034;
  TRKEXP_E_OPEN_ERRORLOG          = 10035;
  TRKEXP_E_BAD_ERRORLOG_PATH      = 10036;
  TRKEXP_E_LOGGING_ERROR          = 10037;
  TRKEXP_E_IMPORT_PERMISSION      = 10038;
  TRKEXP_E_EXPORT_PERMISSION      = 10039;
  TRKEXP_E_NEW_USER_PERMISSION    = 10040;
  TRKEXP_E_CLOSE_ERRORLOG         = 10041;
  TRKEXP_E_NEWCHOICE_SYSFLD       = 10042;
  TRKEXP_E_USER_ALREADY_IN_GROUP  = 10043;
  TRKEXP_E_BAD_STRING_OPTION_ARG  = 10044;
  TRKEXP_E_STRING_TOO_LONG        = 10045;
  TRKEXP_E_EXTRA_FIELDS           = 10046;
  TRKEXP_NUMBER_OF_ERROR_CODES    = TRKEXP_E_EXTRA_FIELDS;

  TRK_INTERNAL_ERROR_CODE_BASE    = 20000;
  TRK_E_INTERNAL_ERROR            = TRK_INTERNAL_ERROR_CODE_BASE;
  TRK_E_FUNCTION_NOT_LOADED       = 20001;
  TRK_E_TTK_NOT_LOADED            = 20002;

  // Values for enum DBMSLoginMode
  TRK_USE_INI_FILE_DBMS_LOGIN  = 0;
  TRK_USE_SPECIFIED_DBMS_LOGIN = 1;
  TRK_USE_DEFAULT_DBMS_LOGIN   = 2;

  // Values for enum _TrkAttributeId
  TRK_TRKTOOL_ATTRIBUTE_ID_BASE = 0;
  TRK_CANCEL_INTRAY             = 1;
  TRK_CANCEL_QUERY              = 2;
  TRK_CANCEL_IMPORT             = 3;
  TRK_NO_KEEP_ALIVE             = 4;
  TRK_NO_TIMER                  = 5;
  TRK_USER_ATTRIBUTE_ID_BASE    = 1000;

  // Values for enum _TrkFieldType
  TRK_FIELD_TYPE_NONE         = 0;
  TRK_FIELD_TYPE_CHOICE       = 1;
  TRK_FIELD_TYPE_STRING       = 2;
  TRK_FIELD_TYPE_NUMBER       = 3;
  TRK_FIELD_TYPE_DATE         = 4;
  TRK_FIELD_TYPE_SUBMITTER    = 5;
  TRK_FIELD_TYPE_OWNER        = 6;
  TRK_FIELD_TYPE_USER         = 7;
  TRK_FIELD_TYPE_ELAPSED_TIME = 8;

  // Values for enum _TrkFieldAccessMode
  TRK_READ_ONLY  = 0;
  TRK_READ_WRITE = 2;

  // Values for enum _TrkFileStorageMode
  TRK_FILE_BINARY = 0;
  TRK_FILE_ASCII  = 1;

  // Values for enum _TrkEmbeddedQuote
  TRK_DOUBLE_QUOTE    = 1;
  TRK_BACKSLASH_QUOTE = 2;

  // Values for enum _TrkDateFormat
  TRK_CONTROL_PANEL_DATE = 1;
  TRK_DBASE_FORMAT       = 2;

  // Values for enum _TrkTimeFormat
  TRK_CONTROL_PANEL_TIME  = 1;
  TRK_24HOUR              = 2;
  TRK_24HOUR_LEADING_ZERO = 3;

  // Values for enum _TrkDateOption
  TRK_FAIL_DATE        = 0;
  TRK_SET_CURRENT      = 1;
  TRK_SET_TO_SPECIFIED = 2;

  // Values for enum _TrkChoiceOption
  TRK_FAIL_CHOICE    = 0;
  TRK_DEFAULT_CHOICE = 1;
  TRK_NEW_CHOICE     = 2;

  // Values for enum _TrkUserOption
  TRK_FAIL_USER = 0;
  TRK_ADD_USER  = 1;

  // Values for enum _TrkNumberOption
  TRK_FAIL_NUMBER    = 0;
  TRK_DEFAULT_NUMBER = 1;

  // Values for enum _TrkCallbackMessage
  TRK_MSG_API_TRACE           = 1;
  TRK_MSG_API_EXIT            = 2;
  TRK_MSG_ODBC_ERROR          = 3;
  TRK_MSG_INVALID_FIELD_VALUE = 4;
  TRK_MSG_DATA_TRUNCATED      = 5;
  TRK_MSG_FORCE_LOGOUT        = 6;
  TRK_MSG_IMPORT_ERROR        = 7;
  TRK_MSG_INTRAY_PROGRESS     = 8;
  TRK_MSG_QUERY_PROGRESS      = 9;
  TRK_MSG_IMPORT_PROGRESS     = 10;
  TRK_LAST_CALLBACK_MSG       = 11;

  // Values for enum _TrkCallbackReturnCode
  TRK_MSG_NOT_HANDLED = 0;
  TRK_MSG_HANDLED     = 1;

  // Values for enum _TrkLinkOrder
  TRK_LIST_ADD_HEAD = 0;
  TRK_LIST_ADD_TAIL = 1;


/// get TRK and EXP error message text
function TRKErrorStr(anError: trkError): string;


type
  /// exception raised if any error occured in TPVCSTracker class
  TPVCSTrackerException = class(Exception);

{$A4} // we need pointer aligned functions for all stdcall initialization
  /// easy PVCS Tracker access
  TPVCSTracker = class
  protected // PVCS dynamic library access functions and values
    fHandle: TRK_HANDLE;
    fRecHandle: TRK_RECORD_HANDLE;
    fNoteHandle: TRK_NOTE_HANDLE; // synchronized with current fRecHandle
    fAttFileHandle: TRK_ATTFILE_HANDLE; // synch. with current fRecHandle
    fAssocHandle: TRK_ASSOC_HANDLE;     // synch. with current fRecHandle
    FRecordCreate,
    FRecordOpened,
    FRecordUpdate,
    FLogged: boolean;
    TRKHandle: HMODULE; { Handle to the TRK library }
    EXPHandle: HMODULE; { Handle to the EXP library }
    tmp: array[byte] of AnsiChar; // used internaly for temporary PAnsiChar buf

    // TrkHandleAlloc must be called before all other Tracker API
    // functions.  TrkHandleFree closes files and releases all
    // associated memory.  Pass the defined constant TRK_VERSION_ID
    // for the version ID.
    fHandleAlloc: function(
      trkVersionID:       TRK_VERSION_ID_TYPE;
      out pTrkHandle:     TRK_HANDLE ): trkError; stdcall;
    fHandleFree: function( var pTrkHandle: TRK_HANDLE ): trkError; stdcall;
    // TrkProjectLogin requires a valid trkHandle, obtained using
    // TrkHandleAlloc.  DBMSLoginMode should be set to one of the three
    // values listed below.  All other arguments may optionally be NULL,
    // indicating that the value should be read from the current .INI
    // file (see also TrkSetIniFile).  Updated values will be written
    // to the current .INI file upon successful login.
    // Successful login is prerequisite to all Tracker API functions
    // except where noted otherwise.
    fProjectLogin: function( trkHandle: TRK_HANDLE;
      userName:           TRK_CSTR;
      password:           TRK_CSTR;
      projectName:        TRK_CSTR;
      DBMSType:           TRK_CSTR;
      DBMSName:           TRK_CSTR;
      DBMSUserName:       TRK_CSTR;
      DBMSPassword:       TRK_CSTR;
      DBMSLoginMode:      TRK_DBMS_LOGIN_MODE ): trkError; stdcall;
    fProjectLoginEx: function( trkHandle: TRK_HANDLE;
      userName:           TRK_CSTR;
      password:           TRK_CSTR;
      projectName:        TRK_CSTR;
      serverName:         TRK_CSTR ): trkError; stdcall;
    fProjectLogout: function( trkHandle: TRK_HANDLE ): trkError; stdcall;
    // A default .INI file will be used if not otherwise specified.
    // Either of these two functions may be called before login.
    fSetIniFile: function( trkHandle: TRK_HANDLE;
      filename:           TRK_CSTR ): trkError; stdcall;
    fGetIniFile: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;                   
      filename:           TRK_STR ): trkError; stdcall;
    // Provide a general mechanism for storing (ID,value) pairs.
    // Attributes with ID values of less than 1000 are used by
    // Tracker.  Attributes with ID values of 1000 and greater
    // may be set and retrieved by clients for any purpose.
    // Either of these two functions may be called before login.
    fSetNumericAttribute: function( trkHandle: TRK_HANDLE;
      attributeId:        cardinal;
      value:              cardinal ): trkError; stdcall;
    fGetNumericAttribute: function( trkHandle: TRK_HANDLE;
      attributeId:        cardinal;
      out pValue:         cardinal ): trkError; stdcall;
    // Provide an enumeration of all Projects in the current DB.
    // TrkInitProjectList initializes an in-memory list of
    // project names, which are accessed sequentially using
    // TrkGetNextProject.
    // These two functions may be called before login.  However, the
    // interpretation of the arguments differs depending on which mode
    // of operation is used:
    // If called before login, TrkInitProjectList reads all of its
    // input arguments, where they will be interpreted exactly as in
    // TrkProjectLogin.  This mode of operation may be useful, for
    // example, in preparing a login dialog.
    // If called after a successful login, the values for all
    // arguments, other than trkHandle, are ignored.
    fInitProjectList: function( trkHandle: TRK_HANDLE;
      DBMSType:           TRK_CSTR;
      DBMSName:           TRK_CSTR;
      DBMSUserName:       TRK_CSTR;
      DBMSPassword:       TRK_CSTR;
      DBMSLoginMode:      TRK_DBMS_LOGIN_MODE ): trkError; stdcall;
    fInitProjectListEx: function( trkHandle: TRK_HANDLE;
      serverName:         TRK_CSTR ): trkError; stdcall;
    fGetNextProject: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      projectName:        TRK_STR ): trkError; stdcall;
    // Provide an enumeration of all DBMS Types known to Tracker.
    // May be called before login.
    fInitDBMSTypeList: function( trkHandle: TRK_HANDLE ): trkError; stdcall;
    fGetNextDBMSType: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      DBMSType:           TRK_STR ): trkError; stdcall;
    // Provide access to current login information
    fGetLoginUserName: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      userName:           TRK_STR ): trkError; stdcall;
    fGetLoginProjectName: function( trkHandle: TRK_HANDLE;
    bufferSize:         cardinal;
    projectName:        TRK_STR ): trkError; stdcall;
    fGetLoginDBMSType: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      DBMSType:           TRK_STR ): trkError; stdcall;
    fGetLoginDBMSName: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      DBMSName:           TRK_STR ): trkError; stdcall;
    // Provide an enumeration of all Record Types (SCR, Time)
    // defined for the current project.
    fInitRecordTypeList: function( trkHandle: TRK_HANDLE ): trkError; stdcall;
    fGetNextRecordType: function( trkHandle: TRK_HANDLE;
      out pRecordType:    TRK_RECORD_TYPE ): trkError; stdcall;
    fGetRecordTypeName: function( trkHandle: TRK_HANDLE;
      recordType:         TRK_RECORD_TYPE;
      bufferSize:         cardinal;
      recordTypeName:     TRK_STR ): trkError; stdcall;
    // Provide an enumeration of all Fields defined in the
    // current project.
    fInitFieldList: function( trkHandle: TRK_HANDLE;
      recordType:         TRK_RECORD_TYPE ): trkError; stdcall;
    fGetNextField: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      fieldName:          TRK_STR;
      out pFieldType:     TRK_FIELD_TYPE ): trkError; stdcall;
    // Provide general information about Fields.
    fGetFieldType: function( trkHandle: TRK_HANDLE;
      fieldName:          TRK_CSTR;
      recordType:         TRK_RECORD_TYPE;
      out pFieldType:     TRK_FIELD_TYPE ): trkError; stdcall;
    fGetFieldMaxDataLength: function( trkHandle: TRK_HANDLE;
      fieldName:          TRK_CSTR;
      recordType:         TRK_RECORD_TYPE;
      out pMaxDataLength: cardinal ): trkError; stdcall;
    fGetFieldDefaultStringValue: function( trkHandle: TRK_HANDLE;
      fieldName:          TRK_CSTR;
      recordType:         TRK_RECORD_TYPE;
      bufferSize:         cardinal;
      defaultValue:       TRK_STR ): trkError; stdcall;
    fGetFieldDefaultNumericValue: function( trkHandle: TRK_HANDLE;
      fieldName:          TRK_CSTR;
      recordType:         TRK_RECORD_TYPE;
      out pDefaultValue:  cardinal ): trkError; stdcall;
    fGetFieldRange: function( trkHandle: TRK_HANDLE;
      fieldName:          TRK_CSTR;
      recordType:         TRK_RECORD_TYPE;
      out pMinValue:      cardinal;
      out pMaxValue:      cardinal ): trkError; stdcall;
    // Provide an enumeration of all stored Queries defined in the
    // current project.
    fInitQueryNameList: function( trkHandle: TRK_HANDLE ): trkError; stdcall;
    fGetNextQueryName: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      queryName:          TRK_STR;
      out pRecordType:    TRK_RECORD_TYPE ): trkError; stdcall;
    fGetQueryRecordType: function( trkHandle: TRK_HANDLE;
      queryName:          TRK_CSTR;
      out pRecordType:    TRK_RECORD_TYPE ): trkError; stdcall;
    // Provide an enumeration of all Users defined in the current project
    fInitUserList: function( trkHandle: TRK_HANDLE ): trkError; stdcall;
    fGetNextUser: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      userName:           TRK_STR ): trkError; stdcall;
    // Provide general information about Users
    fGetUserFullName: function( trkHandle: TRK_HANDLE;
      userName:           TRK_CSTR;
      bufferSize:         cardinal;
      fullName:           TRK_STR ): trkError; stdcall;
    fGetUserEmail: function( trkHandle: TRK_HANDLE;
      userName:           TRK_CSTR;
      bufferSize:         cardinal;
      emailAddress:       TRK_STR ): trkError; stdcall;
    // Provide an enumeration of all values for the specified Choice Field
    fInitChoiceList: function( trkHandle: TRK_HANDLE;
      fieldName:          TRK_CSTR;
      recordType:         TRK_RECORD_TYPE ): trkError; stdcall;
    fGetNextChoice: function( trkHandle: TRK_HANDLE;
      bufferSize:         cardinal;
      choiceName:         TRK_STR ): trkError; stdcall;
    // Provide a Transaction ID representing the most recent
    // change to the current project.
    // A Transaction ID represents the exact date and time when
    // a change is made in a DB.  TrkGetProjectTransactionID
    // provides a Transaction ID which represents the most
    // recent update to any record of the specified type in
    // the DB.  Note that separate Transaction ID domains exist
    // for each record type.
    fGetProjectTransactionID: function(  trkHandle: TRK_HANDLE;
      recordType:         TRK_RECORD_TYPE;
      out pTransactionID: TRK_TRANSACTION_ID ): trkError; stdcall;
    // Tracker Record Handles are used in place of Tracker API
    // Handles in those functions below which operate on Record
    // data.  TrkRecordHandleAlloc must be called first to create
    // a record handle.  TrkRecordHandleFree releases all memory
    // associated with that handle.
    // If desired, multiple Record Handles may be used concurrently.
    fRecordHandleAlloc: function( trkHandle: TRK_HANDLE;
      out pTrkRecordHandle: TRK_RECORD_HANDLE ): trkError; stdcall;
    fRecordHandleFree: function(
      var pTrkRecordHandle: TRK_RECORD_HANDLE ): trkError; stdcall;
    // Provide access to sets of records from the DB.  Two functions
    // TrkQueryInitRecordList and TrkInTrayInitRecordList create
    // in-memory sets of records, which are accessed sequentially
    // using TrkGetNextRecord.  TrkGetNextRecord can be thought of
    // as making a record 'current' in that subsequent calls to the
    // various record access functions will operate on data for that
    // particular record.
    // TrkQueryInitRecordList and TrkInTrayInitRecordList each take
    // an optional input Transaction ID.  If provided, only records
    // which were modified after the time of the specified
    // Transaction ID will be included in the output set.  In
    // addition, a Transaction ID is returned, which represents the
    // time of the most recent update to the DB.
    fQueryInitRecordList: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      queryName:          TRK_CSTR;
      transactionID:      TRK_TRANSACTION_ID;
      NewTransactionID:   PTRK_TRANSACTION_ID ): trkError; stdcall;
    fInTrayInitRecordList: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      recordType:         TRK_RECORD_TYPE;
      transactionID:      TRK_TRANSACTION_ID;
      NewTransactionID:    PTRK_TRANSACTION_ID ): trkError; stdcall;
    fGetNextRecord: function(
      trkRecordHandle:    TRK_RECORD_HANDLE ): trkError; stdcall;
    // TrkQueryGetSingleRecord provides access to an individual
    // Record given its ID number and Type.  The record will be
    // made 'current' in the same sense as TrkGetNextRecord.
    fGetSingleRecord: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      recordId:           cardinal;
      recordType:         TRK_RECORD_TYPE ): trkError; stdcall;
    // Submit a new record to the DB.  TrkNewRecordBegin should
    // be called first to clear the current in-memory record,
    // i.e. to set all data to their default values.
    // After all fields, etc. have been set to their desired
    // values, TrkNewRecordCommit is called to actually add the
    // record to the DB.  The transaction may be cancelled any
    // time before the commit with TrkRecordCancelTransaction.
    fNewRecordBegin: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      recordType:         TRK_RECORD_TYPE ): trkError; stdcall;
    fNewRecordCommit: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pNewTransactionID:  TRK_TRANSACTION_ID ): trkError; stdcall;
    fRecordCancelTransaction: function(
      trkRecordHandle:    TRK_RECORD_HANDLE ): trkError; stdcall;
    // Update a record in the DB.  TrkUpdateRecordBegin populates
    // all fields of the current Record with data from the DB.
    // TrkUpdateRecordBegin also locks the record in the DB to
    // prevent concurrent access.
    // After the desired changes to fields, etc. have been made,
    // TrkUpdateRecordCommit is called to update the record in the
    // DB.  The transaction may be cancelled any time before the
    // commit with TrkRecordCancelTransaction.
    fUpdateRecordBegin: function(
      trkRecordHandle:    TRK_RECORD_HANDLE ): trkError; stdcall;
    fUpdateRecordCommit: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pNewTransactionID:  TRK_TRANSACTION_ID ): trkError; stdcall;
    // Remove a record from the DB.
    fDeleteRecord: function(
      trkRecordHandle:    TRK_RECORD_HANDLE ): trkError; stdcall;
    // Provide miscellaneous information on the current Record.
    fGetRecordTransactionID: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pSubmitTransactionID: TRK_TRANSACTION_ID;
      out pUpdateTransactionID: TRK_TRANSACTION_ID ): trkError; stdcall;
    fGetRecordRecordType: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pRecordType:     TRK_RECORD_TYPE ): trkError; stdcall;
    // Get or set data fields in a record.
    // "Set" functions only modify the in-memory copy of the data.
    fGetNumericFieldValue: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      fieldName:          TRK_CSTR;
      out pFieldValue:    cardinal ): trkError; stdcall;
    fGetStringFieldValue: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      fieldName:          TRK_CSTR;
      fieldValueBufferSize: cardinal;
      fieldValue:         TRK_STR ): trkError; stdcall;
    fSetNumericFieldValue: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      fieldName:          TRK_CSTR;
      fieldValue:         cardinal ): trkError; stdcall;
    fSetStringFieldValue: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      fieldName:          TRK_CSTR;
      fieldValue:         TRK_CSTR ): trkError; stdcall;
    // Provide a Transaction ID representing the most recent
    // change to a particular Field in the current record.
    fGetFieldTransactionID: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      fieldName:          TRK_CSTR;
      out pTransactionID: TRK_TRANSACTION_ID ): trkError; stdcall;
    // Provide permission information on a Record which is about
    // to be Submitted to or Updated in the DB.
    // Note that permissions depend on the state of the record
    // in the DB.  For example, a field may be modifiable upon Submit
    // but not Update, and vice-versa.
    fGetFieldAccessRights: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      fieldName:          TRK_CSTR;
      out pAccessMode:    TRK_ACCESS_MODE ): trkError; stdcall;
    // Get or set data in a record's Description.
    // TrkGetDescriptionData and TrkSetDescriptionData may be
    // called multiple times to read data which exceeds the size
    // of the caller's buffer.
    fGetDescriptionDataLength: function(
      trkRecordHandle:     TRK_RECORD_HANDLE;
      out pDataBufferSize: cardinal ): trkError; stdcall;
    fGetDescriptionData: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;          
      maxBufferSize:      cardinal;                   
      data:               TRK_STR;                    
      out pDataRemaining: cardinal ): trkError; stdcall;
    fSetDescriptionData: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      currentBufferSize:  cardinal;
      data:               TRK_CSTR;
      dataRemaining:      cardinal ): trkError; stdcall;
    // Provide permission information.
    fGetDescriptionAccessRights: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pAccessMode:    TRK_ACCESS_MODE ): trkError; stdcall;
    // Provide a Transaction ID representing the most recent
    // change to the description.
    fGetDescriptionTransactionID: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pTransactionID:     TRK_TRANSACTION_ID ): trkError; stdcall;
    // Tracker Note Handles are used in place of Tracker Record
    // Handles in those functions below which operate on Note
    // data.  TrkNoteHandleAlloc must be called first to create
    // a Note Handle.  TrkNoteHandleFree releases all memory
    // associated with that handle.
    // If desired, multiple Note Handles may be used concurrently.
    fNoteHandleAlloc: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pTrkNoteHandle: TRK_NOTE_HANDLE ): trkError; stdcall;
    fNoteHandleFree: function(
      var pTrkNoteHandle: TRK_NOTE_HANDLE ): trkError; stdcall;
    // TrkInitNoteList initializes an in-memory list of entries which
    // are accessed sequentially.  TrkGetNextNote is used to read
    // the current Note (including the first, if present).
    // TrkGetNextNote can be thought of as making a note 'current'
    // in that subsequent calls to the various note access functions
    // will operate on data for that particular Note.  TrkAddNewNote
    // is used to insert a new Note in the list at the current
    // position, (making the new note current).  A new Note may be
    // appended to the list by adding it after the last Note.
    fInitNoteList: function(
      trkNoteHandle:      TRK_NOTE_HANDLE ): trkError; stdcall;
    fGetNextNote: function(
        trkNoteHandle:      TRK_NOTE_HANDLE ): trkError; stdcall;
    fAddNewNote: function(
      trkNoteHandle:      TRK_NOTE_HANDLE ): trkError; stdcall;
    fDeleteNote: function(
      trkNoteHandle:      TRK_NOTE_HANDLE ): trkError; stdcall;
    // Get or set data in a Note.
    // TrkGetNoteData and TrkSetNoteData may be called multiple
    // times to read data which exceeds the size of the caller's buffer
    // "Set" functions only modify the in-memory copy of the data.
    fGetNoteTitle: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;      
      bufferSize:         cardinal;             
      noteTitle:          TRK_STR ): trkError; stdcall;
    fSetNoteTitle: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      noteTitle:          TRK_CSTR  ): trkError; stdcall;
    fGetNoteAuthor: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      bufferSize:         cardinal;
      authorName:         TRK_STR ): trkError; stdcall;
    fSetNoteAuthor: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      authorName:         TRK_CSTR ): trkError; stdcall;
    fGetNoteCreateTime: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      out pCreateTime:    TRK_TIME ): trkError; stdcall;
    fSetNoteCreateTime: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      createTime:         TRK_TIME ): trkError; stdcall;
    fGetNoteModifyTime: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      out pModifyTime:    TRK_TIME ): trkError; stdcall;
    fSetNoteModifyTime: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      modifyTime:         TRK_TIME ): trkError; stdcall;
    fGetNoteDataLength: function(
      trkNoteHandle:       TRK_NOTE_HANDLE;
      out pDataBufferSize: cardinal ): trkError; stdcall;
    fGetNoteData: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      maxBufferSize:      cardinal;
      noteData:           TRK_STR;
      out pDataRemaining: cardinal ): trkError; stdcall;
    fSetNoteData: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      currentBufferSize:  cardinal;
      noteData:           TRK_CSTR;
      dataRemaining:      cardinal ): trkError; stdcall;
    // Provide a Transaction ID representing the most recent
    // change to a particular Note in the current record.
    fGetNoteTransactionID: function(
      trkNoteHandle:      TRK_NOTE_HANDLE;
      out pTransactionID: TRK_TRANSACTION_ID ): trkError; stdcall;
    // Tracker Attached File Handles are used in place of Tracker Record
    // Handles in those functions below which operate on Attached File
    // data.  TrkAttachedFileHandleAlloc must be called first to create
    // an Attached File Handle.  TrkAttachedFileHandleFree releases all
    // memory associated with that handle.
    // If desired, multiple Attached File Handles may be used concurrently.
    fAttachedFileHandleAlloc: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pTrkAttFileHandle:  TRK_ATTFILE_HANDLE ): trkError; stdcall;
    fAttachedFileHandleFree: function(
      var pTrkAttFileHandle:  TRK_ATTFILE_HANDLE ): trkError; stdcall;
    // TrkInitAttachedFileList initializes an in-memory list of the Files
    // which are attached to the current record, and are accessed
    // sequentially.  TrkGetNextAttachedFile is used to make each
    // successive Attached File current.  Subsequent calls to the various
    // AttFile access functions will operate on data for the current
    // Attached File.  TrkAddNewAttachedFile is used to insert
    // a new AttachedFile in the list at the current position, (making
    // the new Attached File current).  A new Attached File may be
    // appended to the list by adding it after the last Attached File.
    fInitAttachedFileList: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE ): trkError; stdcall;
    fGetNextAttachedFile: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE ): trkError; stdcall;
    fAddNewAttachedFile: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE;
      filename:           TRK_CSTR;
      storageMode:        TRK_FILE_STORAGE_MODE ): trkError; stdcall;
    fDeleteAttachedFile: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE ): trkError; stdcall;
    // Extract or get information about an Attached File.  The function
    // TrkExtractAttachedFile copies the file contents out of the
    // database and into the caller-specified file name.  If the file
    // already exists, it is overwritten.
    fGetAttachedFileName: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE;
      bufferSize:         cardinal;
      filename:           TRK_STR ): trkError; stdcall;
    fGetAttachedFileTime: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE;
      out pTimestamp:     TRK_TIME ): trkError; stdcall;
    fGetAttachedFileStorageMode: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE;
      out pStorageMode:   TRK_FILE_STORAGE_MODE ): trkError; stdcall;
    fExtractAttachedFile: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE;
      filename:           TRK_CSTR ): trkError; stdcall;
    // Provide a Transaction ID representing when a particular
    // Attached File was added to the current record.
    fGetAttachedFileTransactionID: function(
      trkAttFileHandle:   TRK_ATTFILE_HANDLE;
      out pTransactionID: TRK_TRANSACTION_ID ): trkError; stdcall;
    // Tracker Association Handles are used in place of Tracker
    // Record Handles in those functions below which operate on
    // Module Association data.  TrkAssociationHandleAlloc must
    // be called first to create an Association Handle.
    // TrkAssociationHandleFree releases all memory associated with
    // that handle.
    // If desired, multiple Association Handles may be used
    // concurrently.
    fAssociationHandleAlloc: function(
      trkRecordHandle:    TRK_RECORD_HANDLE;
      out pTrkAssociationHandle: TRK_ASSOC_HANDLE ): trkError; stdcall;
    fAssociationHandleFree: function(
      var pTrkAssociationHandle: TRK_ASSOC_HANDLE ): trkError; stdcall;
    // TrkInitAssociationList initializes an in-memory list of the
    // Modules which are associated with the current record, and are
    // accessed sequentially.  TrkGetNextAssociation is used to make
    // each successive Association current.  Subsequent calls to the
    // various Association access functions will operate on data for
    // the current Association.  TrkAddNewAssociation is used to
    // insert a new Association in the list at the current position,
    // (making the new Association current).  A new Association may
    // be appended to the list by adding it after the last Association.
    fInitAssociationList: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE ): trkError; stdcall;
    fGetNextAssociation: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE ): trkError; stdcall;
    fAddNewAssociation: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE ): trkError; stdcall;
    fDeleteAssociation: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE ): trkError; stdcall;
    // Get or set data in an Association.
    // TrkGetAssociationText and TrkSetAssociationText may be
    // called multiple times to read data which exceeds the size
    // of the caller's buffer.
    // "Set" functions only modify the in-memory copy of the data.
    fGetAssociationModuleName: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      bufferSize:         cardinal;
      moduleName:         TRK_STR ): trkError; stdcall;
    fSetAssociationModuleName: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      moduleName:         TRK_CSTR ): trkError; stdcall;
    fGetAssociationUser: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      bufferSize:         cardinal;
      userName:           TRK_STR ): trkError; stdcall;
    fSetAssociationUser: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      userName:           TRK_CSTR ): trkError; stdcall;
    fGetAssociationRevisionFound: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      bufferSize:         cardinal;
      revisionFound:      TRK_STR ): trkError; stdcall;
    fSetAssociationRevisionFound: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      revisionFound:      TRK_CSTR ): trkError; stdcall;
    fGetAssociationRevisionFixed: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      bufferSize:         cardinal;
      revisionFixed:      TRK_STR ): trkError; stdcall;
    fSetAssociationRevisionFixed: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      revisionFixed:      TRK_CSTR ): trkError; stdcall;
    fGetAssociationTimeFound: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      out pTimeFound:       TRK_TIME ): trkError; stdcall;
    fSetAssociationTimeFound: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      timeFound:          TRK_TIME ): trkError; stdcall;
    fGetAssociationTimeFixed: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      out pTimeFixed:       TRK_TIME ): trkError; stdcall;
    fSetAssociationTimeFixed: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      timeFixed:          TRK_TIME ): trkError; stdcall;
    fGetAssociationTextLength: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      out pDataBufferSize:  cardinal ): trkError; stdcall;
    fGetAssociationText: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      maxBufferSize:      cardinal;
      description:        TRK_STR;
      out pDataRemaining: cardinal ): trkError; stdcall;
    fSetAssociationText: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      currentBufferSize:  cardinal;
      description:        TRK_CSTR;
      dataRemaining:      cardinal ): trkError; stdcall;
    // Provide a Transaction ID representing when a particular
    // Association was added to the current record.
    fGetAssociationTransactionID: function(
      trkAssociationHandle: TRK_ASSOC_HANDLE;
      out pTransactionID:     TRK_TRANSACTION_ID ): trkError; stdcall;
    // Callback registration.
    // The linkOrder parameter should be set to either TRK_LIST_ADD_HEAD
    // or TRK_LIST_ADD_TAIL to indicate whether the callback should be
    // called first or last.
    // The userData value will be supplied back to the client as one
    // of the arguments in the callback.
    // This function may be called before login.
    fRegisterCallback: function(
      trkHandle: TRK_HANDLE;
      pCallbackFunction:  PTRKCALLBACK;
      linkOrder:          TRK_LIST_LINK_ORDER;
      userData:           cardinal ): trkError; stdcall;
    // Remove a function from the list of callbacks.
    // This function may be called before login.
    fUnregisterCallback: function(
      trkHandle: TRK_HANDLE;
      pCallbackFunction:  PTRKCALLBACK ): trkError; stdcall;

    // The Tracker Export Handle is used in place of Tracker
    // Handles in those functions below which perform export
    // operations.
    // TrkExportHandleAlloc must be called first to create
    // an Export Handle.  TrkExportHandleFree releases all
    // memory associated with that handle.
    // If desired, multiple Export Handles may be used
    // concurrently.
    fExportHandleAlloc: function(
      trkHandle: TRK_HANDLE;
      out pTrkExportHandle:   TRK_EXPORT_HANDLE ): trkError; stdcall;
    fExportHandleFree: function(
      var pTrkExportHandle:   TRK_EXPORT_HANDLE ): trkError; stdcall;
    // Provide higher level functions which export records to a
    // file using a fixed format.
    // The TrkExportRecordsBegin function is called first to set
    // the filename, export set, delimiter character, separator
    // character, the type of quote embedding to use (either \" or
    // ""), as well as the format for date and time fields.  If the
    // "trackerFormat" argument is set to TRUE, then the remaining
    // arguments should be set to zero and will be ignored.  The
    // "trackerFormat" argument must be set to TRUE if the export
    // file will be imported back into Tracker.
    // The TrkExportOneRecord function exports one record.  The
    // TrkExportRecordsClose function must be called to complete
    // the export operation.
    fExportRecordsBegin: function(
      trkExportHandle:    TRK_EXPORT_HANDLE;
      recordType:         TRK_RECORD_TYPE;
      fileToWriteTo:      TRK_CSTR;
      trackerFormat:      TRK_BOOL;
      delimiterCharacter: TRK_CHAR;
      separatorCharacter: TRK_CHAR;
      embeddedQuoteType:  TRK_EMB_QUOTE;
      dateFormat:         TRK_DATE_FORMAT;
      timeFormat:         TRK_TIME_FORMAT ): trkError; stdcall;
    fExportOneRecord: function(
      trkExportHandle:    TRK_EXPORT_HANDLE;
      trkRecordHandle:    TRK_RECORD_HANDLE ): trkError; stdcall;
    fExportRecordsClose: function(
      trkExportHandle:    TRK_EXPORT_HANDLE ): trkError; stdcall;
    // The Tracker Import Handle is used in place of Tracker
    // Handles in those functions below which perform import
    // operations.
    // TrkImportHandleAlloc must be called first to create an
    // Import Handle.  TrkImportHandleFree releases all memory
    // associated with that handle.
    // If desired, multiple Import Handles may be used
    // concurrently.
    fImportHandleAlloc: function(
      trkHandle: TRK_HANDLE;
      out pTrkImportHandle:   TRK_IMPORT_HANDLE ): trkError; stdcall;
    fImportHandleFree: function(
      var pTrkImportHandle:   TRK_IMPORT_HANDLE ): trkError; stdcall;
    // Provide higher level functions to import records from a
    // file.  Assumes the file was written using the same format as
    // the "TrkExport" functions above and that "trackerFormat" was
    // set to TRUE in the TrkExportRecordsBegin function.
    // TrkImportInit must be called first.  The filename containing
    // the import data is specified in this function call.  In
    // addition, arguments are given to indicate which actions to
    // take when a bad date, choice, user, or out of range number
    // is encountered. If these functions are not called, the
    // default behavior is to fail import.  If the
    // TRK_SET_TO_SPECIFIED date option is used, the "defaultDate"
    // argument must be provided (in Control Panel format - default
    // format is MM/DD/YY).
    // TrkImportNewRecords performs the actual import. Failure to
    // set "trackerFormat" to TRUE on export will result in a
    // failure to import.  The fields being imported will be
    // automatically mapped by type and name.  Fields that don't
    // match any fields in the import project will be discarded.
    // All notes will be imported regardless of note title.
    fImportInit: function(
      trkImportHandle:    TRK_IMPORT_HANDLE;
      fileToReadFrom:     TRK_CSTR;
      trackerFormat:      TRK_BOOL;
      recordType:         TRK_RECORD_TYPE;
      delimiterCharacter: TRK_CHAR;
      separatorCharacter: TRK_CHAR;
      dateFormat:         TRK_DATE_FORMAT;
      timeFormat:         TRK_TIME_FORMAT ): trkError; stdcall;
    fImportSetOptions: function(
      trkImportHandle:    TRK_IMPORT_HANDLE;
      choiceOption:       TRK_CHOICE_OPTION;
      userOption:         TRK_USER_OPTION;
      numberOption:       TRK_NUMBER_OPTION;
      dateOption:         TRK_DATE_OPTION;
      defaultDate:        TRK_CSTR ): trkError; stdcall;
    fImportNewRecords: function(
      trkImportHandle:    TRK_IMPORT_HANDLE;
      errorLogFile:       TRK_CSTR ): trkError; stdcall;
    procedure LoadTRKDll;
    procedure LoadEXPDll;
    procedure Check(rc: cardinal);
    procedure CheckPartial(rc: cardinal);
  private
    function getNumericValue(AttributeID: cardinal): cardinal;
    procedure setNumericValue(AttributeID: cardinal;
      const Value: cardinal); // raise a TPVCSTrackerException if rc is invalid
    procedure GetFields(Fields: TStrings; aRecordType: TRK_RECORD_TYPE=1);
    function getFieldIndexString(FieldIndex: integer): AnsiString;
    procedure setFieldIndexString(FieldIndex: integer; const Value: AnsiString);
    function getFieldIndexNumeric(FieldIndex: integer): cardinal;
    procedure setFieldIndexNumeric(FieldIndex: integer; const Value: cardinal);
    function getFieldNameNumeric(const FieldName: AnsiString): cardinal;
    function getFieldNameString(const FieldName: AnsiString): AnsiString;
    procedure setFieldNameNumeric(const FieldName: AnsiString; const Value: cardinal);
    procedure setFieldNameString(const FieldName, Value: AnsiString);
    function getRecordDescription: AnsiString;
    procedure setRecordDescription(const Value: AnsiString);
    procedure SetRecordOpened(const Value: boolean);
    function getNoteTitle: AnsiString;
    procedure setNoteTitle(const Value: AnsiString);
    function getNoteAuthor: AnsiString;
    function getNoteData: AnsiString;
    procedure setNoteAuthor(const Value: AnsiString);
    procedure setNoteData(const Value: AnsiString);
    function getNoteTime: TDateTime;
    procedure setNoteTime(const Value: TDateTime);
    function getModuleAuthor: AnsiString;
    function getModuleCommit: AnsiString;
    function getModuleFileName: AnsiString;
    function getModuleText: AnsiString;
    procedure setModuleAuthor(const Value: AnsiString);
    procedure setModuleCommit(const Value: AnsiString);
    procedure setModuleFileName(const Value: AnsiString);
    procedure setModuleText(const Value: AnsiString);
    function getFileName: AnsiString;
    function getFileTime: TDateTime;
    function getModuleAction: AnsiString;
    procedure setModuleAction(const Value: AnsiString);
  public
    DBMSName: AnsiString; // leave '' to get defaul DBMS
    DBMSType: AnsiString; // leave '' to get defaul DBMS
    Fields: TStringList; // Objects[]=pFieldType

    constructor Create(DBMSTypes: TStrings = nil);
    destructor Destroy; override;

    // Login functions
    function GetIniFile: AnsiString;
    procedure Login(const userName: AnsiString=''; const password: AnsiString='';
      const projectName: AnsiString='');
    procedure Logout;
    procedure GetLoginInformation(out UserName, ProjectName, DBMSType, DBMSName: AnsiString);
    procedure GetUserInformation(const UserName: AnsiString; out UserFullName, UserEmail: AnsiString);
    procedure GetUsers(Users: TStrings);

    // available function after Login
    function GetProjects(Projects: TStrings): integer; // return logged project index
    procedure GetQueryNames(QueryNames: TStrings); // Objects[]=pRecordType
    function GetRecordTypeName(recordType: TRK_RECORD_TYPE): AnsiString;
    function FieldIsString(FieldIndex: integer): boolean;
    function FieldIsNumeric(FieldIndex: integer): boolean;

    // records navigation / update
    procedure RecordOpen(aRecordId: cardinal); // + Field..[] / RecordDescription
    procedure RecordUpdate(aRecordId: cardinal;
      withModule, withNote: boolean); // + Field..[] + RecordCommit
    function RecordCreate: cardinal; // returns new RecordID + Field..[] + RecordCommit
    function RecordCommit: TRK_TRANSACTION_ID; // close pending RecordUpdate/Create
    procedure RecordCancel; // after RecordOpen(?,true) = cancel transaction
    procedure OpenInBox; // + while NextRecord do ... Field..[]
    procedure OpenQuery(const aQueryName: AnsiString = 'All_SCRs'); // + NextRecord
    procedure FillQuery(SCRList: TStrings; const aQueryName: AnsiString = 'All_SCRs');
    function NextRecord: boolean; // for OpenInBox and OpenInBox
    procedure RecordDelete;

    // notes navigation / update
    function NextNote: boolean; // auto init if first time for this record
    procedure NoteCreate; // add note for current record
    procedure NoteAdd(const aNoteAuthor, aNoteTitle, aNoteData: AnsiString);
    procedure NoteDelete;

    // attached files navigation / update
    function NextFile: boolean; // auto init if first time for this record
    procedure FileAdd(const aFileName: AnsiString); // add attached file for current record
    procedure FileSaveAs(const aDestFileName: AnsiString);
    procedure FileDelete;

    // modules navigation / update
    function NextModule: boolean; // auto init if first time for this record
    procedure ModuleAdd(const aFileName, aAuthor, aAction, aCommit, aText: AnsiString);
    procedure ModuleDelete;

    // global (ID,value) pairs (all ID<1000 are used internaly by PVCS)
    property NumericValue[AttributeID: cardinal]: cardinal read getNumericValue write setNumericValue;

    // true after Login
    property Logged: boolean read FLogged;

    // true after sucessfull RecordOpen, RecordUpdate, RecordCreate or NextRecord
    property RecordOpened: boolean read FRecordOpened write SetRecordOpened;

    // current record Field values read/write access
    property FieldIndexString[FieldIndex: integer]: AnsiString read getFieldIndexString write setFieldIndexString;
    property FieldIndexNumeric[FieldIndex: integer]: cardinal read getFieldIndexNumeric write setFieldIndexNumeric;
    property FieldNameString[const FieldName: AnsiString]: AnsiString read getFieldNameString write setFieldNameString;
    property FieldNameNumeric[const FieldName: AnsiString]: cardinal read getFieldNameNumeric write setFieldNameNumeric;

    // current record global description read/write
    property RecordDescription: AnsiString read getRecordDescription write setRecordDescription;

    // current record note properties read/write
    property NoteTitle: AnsiString read getNoteTitle write setNoteTitle;
    property NoteAuthor: AnsiString read getNoteAuthor write setNoteAuthor;
    property NoteTime: TDateTime read getNoteTime write setNoteTime;
    property NoteData: AnsiString read getNoteData write setNoteData;

    // current record file properties read/write
    property FileName: AnsiString read getFileName;
    property FileTime: TDateTime read getFileTime;

    // current record module properties read/write
    property ModuleFileName: AnsiString read getModuleFileName write setModuleFileName;
    property ModuleAuthor: AnsiString read getModuleAuthor write setModuleAuthor;
    property ModuleAction: AnsiString read getModuleAction write setModuleAction;
    property ModuleCommit: AnsiString read getModuleCommit write setModuleCommit;
    property ModuleText: AnsiString read getModuleText write setModuleText;
  end;
{$A+}


var /// local time to gmt time diff (set in initialization section of this unit)
  ActiveTimeBias: TDateTime = 0; // = GetActiveTimeBias / MinsPerDay;

/// convert a gmt-based c/unix-encoded time into local TDateTime (for display e.g)
function cTimeToLocalDateTime(const cTime: Int64): TDateTime;


implementation

function GetPVCSDirectory: TFileName;
var Key: HKey;
    size: cardinal;
begin
  result := '';
  if RegOpenKey(HKEY_LOCAL_MACHINE, 'SOFTWARE\MERANT\PVCS TrackerLink',Key)=0 then
  try
    Size := 255;
    SetLength(result,255);
    if RegQueryValueEx(Key,'SCCServerPath',nil,nil,pointer(result),@size)=0 then
      SetLength(result,size-1) else
      result := '';
    result := ExtractFilePath(result);
  finally
    RegCloseKey(Key);
  end;
end;

{ TPVCSTracker }

function ProcLoad(const aDllFileName: TFileName; Addr: PPointer; Names: PPChar): HMODULE;
var H: HMODULE;
begin
  result := 0;
  H := SafeLoadLibrary(aDllFileName);
  if H=0 then
    exit;
  repeat
    Addr^ := GetProcAddress(H,Names^);
    if Addr^=nil then
    if Addr^=nil then
      exit;
    inc(Addr);
    inc(Names);
  until Names^=nil;
  result := H;
end;

procedure TPVCSTracker.LoadTRKDll;
const trktooln: array[0..116] of PChar =
  ( 'TrkHandleAlloc','TrkHandleFree','TrkProjectLogin','TrkProjectLoginEx',
    'TrkProjectLogout','TrkSetIniFile','TrkGetIniFile',
    'TrkSetNumericAttribute','TrkGetNumericAttribute',
    'TrkInitProjectList', 'TrkInitProjectListEx', 'TrkGetNextProject',
    'TrkInitDBMSTypeList', 'TrkGetNextDBMSType', 'TrkGetLoginUserName',
    'TrkGetLoginProjectName', 'TrkGetLoginDBMSType', 'TrkGetLoginDBMSName',
    'TrkInitRecordTypeList', 'TrkGetNextRecordType', 'TrkGetRecordTypeName',
    'TrkInitFieldList', 'TrkGetNextField', 'TrkGetFieldType',
    'TrkGetFieldMaxDataLength', 'TrkGetFieldDefaultStringValue',
    'TrkGetFieldDefaultNumericValue', 'TrkGetFieldRange',
    'TrkInitQueryNameList', 'TrkGetNextQueryName',
    'TrkGetQueryRecordType', 'TrkInitUserList', 'TrkGetNextUser',
    'TrkGetUserFullName', 'TrkGetUserEmail', 'TrkInitChoiceList',
    'TrkGetNextChoice', 'TrkGetProjectTransactionID', 'TrkRecordHandleAlloc',
    'TrkRecordHandleFree', 'TrkQueryInitRecordList', 'TrkInTrayInitRecordList',
    'TrkGetNextRecord', 'TrkGetSingleRecord', 'TrkNewRecordBegin',
    'TrkNewRecordCommit', 'TrkRecordCancelTransaction',
    'TrkUpdateRecordBegin', 'TrkUpdateRecordCommit', 'TrkDeleteRecord',
    'TrkGetRecordTransactionID', 'TrkGetRecordRecordType',
    'TrkGetNumericFieldValue', 'TrkGetStringFieldValue', 'TrkSetNumericFieldValue',
    'TrkSetStringFieldValue', 'TrkGetFieldTransactionID',
    'TrkGetFieldAccessRights', 'TrkGetDescriptionDataLength',
    'TrkGetDescriptionData', 'TrkSetDescriptionData',
    'TrkGetDescriptionAccessRights', 'TrkGetDescriptionTransactionID',
    'TrkNoteHandleAlloc', 'TrkNoteHandleFree',
    'TrkInitNoteList', 'TrkGetNextNote', 'TrkAddNewNote', 'TrkDeleteNote',
    'TrkGetNoteTitle', 'TrkSetNoteTitle', 'TrkGetNoteAuthor', 'TrkSetNoteAuthor',
    'TrkGetNoteCreateTime', 'TrkSetNoteCreateTime', 'TrkGetNoteModifyTime',
    'TrkSetNoteModifyTime', 'TrkGetNoteDataLength',
    'TrkGetNoteData', 'TrkSetNoteData',
    'TrkGetNoteTransactionID', 'TrkAttachedFileHandleAlloc',
    'TrkAttachedFileHandleFree', 'TrkInitAttachedFileList',
    'TrkGetNextAttachedFile', 'TrkAddNewAttachedFile',
    'TrkDeleteAttachedFile', 'TrkGetAttachedFileName',
    'TrkGetAttachedFileTime', 'TrkGetAttachedFileStorageMode',
    'TrkExtractAttachedFile', 'TrkGetAttachedFileTransactionID',
    'TrkAssociationHandleAlloc', 'TrkAssociationHandleFree',
    'TrkInitAssociationList', 'TrkGetNextAssociation',
    'TrkAddNewAssociation', 'TrkDeleteAssociation',
    'TrkGetAssociationModuleName', 'TrkSetAssociationModuleName',
    'TrkGetAssociationUser', 'TrkSetAssociationUser',
    'TrkGetAssociationRevisionFound', 'TrkSetAssociationRevisionFound',
    'TrkGetAssociationRevisionFixed', 'TrkSetAssociationRevisionFixed',
    'TrkGetAssociationTimeFound', 'TrkSetAssociationTimeFound',
    'TrkGetAssociationTimeFixed', 'TrkSetAssociationTimeFixed',
    'TrkGetAssociationTextLength', 'TrkGetAssociationText',
    'TrkSetAssociationText', 'TrkGetAssociationTransactionID',
    'TrkRegisterCallback', 'TrkUnregisterCallback', nil);
var dir: TFileName;
begin
  if TRKHandle<>0 then
    exit;
  TRKHandle := ProcLoad('trktooln.dll',@@fHandleAlloc,@trktooln);
  if TRKHandle=0 then begin
    dir := GetCurrentDir;
    SetCurrentDir(GetPVCSDirectory);
    TRKHandle := ProcLoad('trktooln.dll',@@fHandleAlloc,@trktooln);
    SetCurrentDir(dir);
  end;
end;

procedure TPVCSTracker.LoadEXPDll;
const expdlln: array[0..10] of PChar =
  ('TrkExportHandleAlloc', 'TrkExportHandleFree',
   'TrkExportRecordsBegin', 'TrkExportOneRecord', 'TrkExportRecordsClose',
   'TrkImportHandleAlloc', 'TrkImportHandleFree',
   'TrkImportInit', 'TrkImportSetOptions', 'TrkImportNewRecords', nil);
var dir: TFileName;
begin
  if EXPHandle<>0 then
    exit;
  EXPHandle := ProcLoad('expdlln.dll',@@fExportHandleAlloc,@expdlln);
  if EXPHandle=0 then begin
    dir := GetCurrentDir;
    SetCurrentDir(GetPVCSDirectory);
    EXPHandle := ProcLoad('expdlln.dll',@@fExportHandleAlloc,@expdlln);
    SetCurrentDir(dir);
  end;
end;

function TRKErrorStr(anError: trkError): string;
begin
  case anError of
    TRK_SUCCESS:                     result := 'SUCCESS';
    TRK_E_VERSION_MISMATCH:          result := 'VERSION MISMATCH';
    TRK_E_OUT_OF_MEMORY:             result := 'OUT OF MEMORY';
    TRK_E_BAD_HANDLE:                result := 'BAD HANDLE';
    TRK_E_BAD_INPUT_POINTER:         result := 'BAD INPUT POINTER';
    TRK_E_BAD_INPUT_VALUE:           result := 'BAD INPUT VALUE';
    TRK_E_DATA_TRUNCATED:            result := 'DATA TRUNCATED';
    TRK_E_NO_MORE_DATA:              result := 'NO MORE DATA';
    TRK_E_LIST_NOT_INITIALIZED:      result := 'LIST NOT INITIALIZED';
    TRK_E_END_OF_LIST:               result := 'END OF LIST';
    TRK_E_NOT_LOGGED_IN:             result := 'NOT LOGGED IN';
    TRK_E_SERVER_NOT_PREPARED:       result := 'SERVER NOT PREPARED';
    TRK_E_BAD_DATABASE_VERSION:      result := 'BAD DATABASE VERSION';
    TRK_E_UNABLE_TO_CONNECT:         result := 'UNABLE TO CONNECT';
    TRK_E_UNABLE_TO_DISCONNECT:      result := 'UNABLE TO DISCONNECT';
    TRK_E_UNABLE_TO_START_TIMER:     result := 'UNABLE TO START TIMER';
    TRK_E_NO_DATA_SOURCES:           result := 'NO DATA SOURCES';
    TRK_E_NO_PROJECTS:               result := 'NO PROJECTS';
    TRK_E_WRITE_FAILED:              result := 'WRITE FAILED';
    TRK_E_PERMISSION_DENIED:         result := 'PERMISSION DENIED';
    TRK_E_SET_FIELD_DENIED:          result := 'SET FIELD DENIED';
    TRK_E_ITEM_NOT_FOUND:            result := 'ITEM NOT FOUND';
    TRK_E_CANNOT_ACCESS_DATABASE:    result := 'CANNOT ACCESS DATABASE';
    TRK_E_CANNOT_ACCESS_QUERY:       result := 'CANNOT ACCESS QUERY';
    TRK_E_CANNOT_ACCESS_INTRAY:      result := 'CANNOT ACCESS INTRAY';
    TRK_E_CANNOT_OPEN_FILE:          result := 'CANNOT OPEN FILE';
    TRK_E_INVALID_DBMS_TYPE:         result := 'INVALID DBMS TYPE';
    TRK_E_INVALID_RECORD_TYPE:       result := 'INVALID RECORD TYPE';
    TRK_E_INVALID_FIELD:             result := 'INVALID FIELD';
    TRK_E_INVALID_CHOICE:            result := 'INVALID CHOICE';
    TRK_E_INVALID_USER:              result := 'INVALID USER';
    TRK_E_INVALID_SUBMITTER:         result := 'INVALID SUBMITTER';
    TRK_E_INVALID_OWNER:             result := 'INVALID OWNER';
    TRK_E_INVALID_DATE:              result := 'INVALID DATE';
    TRK_E_INVALID_STORED_QUERY:      result := 'INVALID STORED QUERY';
    TRK_E_INVALID_MODE:              result := 'INVALID MODE';
    TRK_E_INVALID_MESSAGE:           result := 'INVALID MESSAGE';
    TRK_E_VALUE_OUT_OF_RANGE:        result := 'VALUE OUT OF RANGE';
    TRK_E_WRONG_FIELD_TYPE:          result := 'WRONG FIELD TYPE';
    TRK_E_NO_CURRENT_RECORD:         result := 'NO CURRENT RECORD';
    TRK_E_NO_CURRENT_NOTE:           result := 'NO CURRENT NOTE';
    TRK_E_NO_CURRENT_ATTACHED_FILE:  result := 'NO CURRENT ATTACHED FILE';
    TRK_E_NO_CURRENT_ASSOCIATION:    result := 'NO CURRENT ASSOCIATION';
    TRK_E_NO_RECORD_BEGIN:           result := 'NO RECORD BEGIN';
    TRK_E_NO_MODULE:                 result := 'NO MODULE';
    TRK_E_USER_CANCELLED:            result := 'USER CANCELLED';
    TRK_E_SEMAPHORE_TIMEOUT:         result := 'SEMAPHORE TIMEOUT';
    TRK_E_SEMAPHORE_ERROR:           result := 'SEMAPHORE ERROR';
    TRK_E_INVALID_SERVER_NAME:       result := 'INVALID SERVER NAME';
    TRK_E_NOT_LICENSED:              result := 'NOT LICENSED';
    TRK_E_RECORD_LOCKED:             result := 'RECORD LOCKED';
    TRK_E_PARTIAL_SUCCESS:           result := 'PARTIAL SUCCESS';
    TRK_E_RECORD_NOT_LOCKED:         result := 'RECORD NOT LOCKED';
    TRKEXP_E_EXPORT_WRONG_VERSION:   result := 'EXPORT WRONG VERSION';
    TRKEXP_E_EXPORTSET_NOT_INIT:     result := 'EXPORTSET NOT INIT';
    TRKEXP_E_NO_EXPSET_NAME:         result := 'NO EXPSET NAME';
    TRKEXP_E_BAD_EXPSET_NAME:        result := 'BAD EXPSET NAME';
    TRKEXP_E_EXPSET_FAIL_CREATE:     result := 'EXPSET FAIL CREATE';
    TRKEXP_E_IMPORTMAP_NOT_INIT:     result := 'IMPORTMAP NOT INIT';
    TRKEXP_E_NO_IMPMAP_NAME:         result := 'NO IMPMAP NAME';
    TRKEXP_E_BAD_IMPMAP_NAME:        result := 'BAD IMPMAP NAME';
    TRKEXP_E_IMPMAP_FAIL_CREATE:     result := 'IMPMAP FAIL CREATE';
    TRKEXP_E_IMP_VALIDATE_FAIL:      result := 'IMP VALIDATE FAIL';
    TRKEXP_E_USER_NOEXIST:           result := 'USER NOEXIST';
    TRKEXP_E_USER_ADD:               result := 'USER ADD';
    TRKEXP_E_IMPORT_NOT_INIT:        result := 'IMPORT NOT INIT';
    TRKEXP_E_BAD_EMBEDDED_QUOTE_ARG: result := 'BAD EMBEDDED QUOTE ARG';
    TRKEXP_E_BAD_DATEFORMAT_ARG:     result := 'BAD DATEFORMAT ARG';
    TRKEXP_E_BAD_TIMEFORMAT_ARG:     result := 'BAD TIMEFORMAT ARG';
    TRKEXP_E_BAD_CHOICE_OPTION_ARG:  result := 'BAD CHOICE OPTION ARG';
    TRKEXP_E_BAD_USER_OPTION_ARG:    result := 'BAD USER OPTION ARG';
    TRKEXP_E_BAD_NUMBER_OPTION_ARG:  result := 'BAD NUMBER OPTION ARG';
    TRKEXP_E_BAD_DATE_OPTION_ARG:    result := 'BAD DATE OPTION ARG';
    TRKEXP_E_ALL_NOTES_SELECTED:     result := 'ALL NOTES SELECTED';
    TRKEXP_E_READ_EXPORTHDR:         result := 'READ EXPORTHDR';
    TRKEXP_E_WRITE_EXPORTHDR:        result := 'WRITE EXPORTHDR';
    TRKEXP_E_READ_RECORDHDR:         result := 'READ RECORDHDR';
    TRKEXP_E_WRITE_RECORDHDR:        result := 'WRITE RECORDHDR';
    TRKEXP_E_WRITE_FIELD:            result := 'WRITE FIELD';
    TRKEXP_E_OPEN_FILE:              result := 'OPEN FILE';
    TRKEXP_E_READ_FIELD:             result := 'READ FIELD';
    TRKEXP_E_READ_FIELD_WRONG_TYPE:  result := 'READ FIELD WRONG TYPE';
    TRKEXP_E_BAD_ITEM_TYPE:          result := 'BAD ITEM TYPE';
    TRKEXP_E_READ_FROM_DB:           result := 'READ FROM DB';
    TRKEXP_E_WRITE_TO_DB:            result := 'WRITE TO DB';
    TRKEXP_E_BAD_DATE:               result := 'BAD DATE';
    TRKEXP_E_BAD_CHOICE:             result := 'BAD CHOICE';
    TRKEXP_E_BAD_NUMBER:             result := 'BAD NUMBER';
    TRKEXP_E_OPEN_ERRORLOG:          result := 'OPEN ERRORLOG';
    TRKEXP_E_BAD_ERRORLOG_PATH:      result := 'BAD ERRORLOG PATH';
    TRKEXP_E_LOGGING_ERROR:          result := 'LOGGING ERROR';
    TRKEXP_E_IMPORT_PERMISSION:      result := 'IMPORT PERMISSION';
    TRKEXP_E_EXPORT_PERMISSION:      result := 'EXPORT PERMISSION';
    TRKEXP_E_NEW_USER_PERMISSION:    result := 'NEW USER PERMISSION';
    TRKEXP_E_CLOSE_ERRORLOG:         result := 'CLOSE ERRORLOG';
    TRKEXP_E_NEWCHOICE_SYSFLD:       result := 'NEWCHOICE SYSFLD';
    TRKEXP_E_EXTRA_FIELDS:           result := 'EXTRA FIELDS';
    TRK_E_INTERNAL_ERROR:            result := 'INTERNAL ERROR';
    TRK_E_FUNCTION_NOT_LOADED:       result := 'FUNCTION NOT LOADED';
    TRK_E_TTK_NOT_LOADED:            result := 'TTK NOT LOADED';
    else result := 'unknown';
  end;
end;

procedure TPVCSTracker.Check(rc: cardinal);
begin
  if rc<>TRK_SUCCESS then
    raise TPVCSTrackerException.CreateFmt('PVCS Tracker error %d (%s)',
      [rc,TRKErrorStr(rc)]);
end;

procedure TPVCSTracker.CheckPartial(rc: cardinal);
begin
  if (rc<>TRK_SUCCESS) and (rc<>TRK_E_PARTIAL_SUCCESS) then
    raise TPVCSTrackerException.CreateFmt('PVCS Tracker error %d (%s)',
      [rc,TRKErrorStr(rc)]);
end;

constructor TPVCSTracker.Create(DBMSTypes: TStrings = nil);
begin
  LoadTRKDll;
  if TRKHandle=0 then
    raise TPVCSTrackerException.Create('Failed to load PVCS Tracker dll');
  Check(fHandleAlloc(TRK_VERSION_ID, fHandle));
  Check(fRecordHandleAlloc(fHandle, fRecHandle));
  if DBMSTypes<>nil then begin
    DBMSTypes.Clear;
    if fInitDBMSTypeList(fHandle)=TRK_SUCCESS then
      while fGetNextDBMSType(fHandle,sizeof(tmp),tmp)=TRK_SUCCESS do
        DBMSTypes.Add(tmp);
  end;
  Fields := TStringList.Create;
end;

destructor TPVCSTracker.Destroy;
begin
  if (fHandle<>0) and (TRKHandle<>0) then
  try
    RecordOpened := false; // force release fAttFileHandle and fNoteHandle
    Check(fRecordHandleFree(fRecHandle));
    Logout;
    Check(fHandleFree(fHandle));
  except
    on TPVCSTrackerException do // exception don't freeze memory release
      FLogged := false;
  end;
  FreeAndNil(Fields);
  if TRKHandle<>0 then
    FreeLibrary(TRKHandle);
  if EXPHandle<>0 then
    FreeLibrary(EXPHandle);
  inherited;
end;

function TPVCSTracker.FieldIsNumeric(FieldIndex: integer): boolean;
begin
  result := not FieldIsString(FieldIndex);
end;

function TPVCSTracker.FieldIsString(FieldIndex: integer): boolean;
begin
  result := TRK_FIELD_TYPE(Fields.Objects[FieldIndex]) in
    [TRK_FIELD_TYPE_CHOICE, TRK_FIELD_TYPE_STRING, TRK_FIELD_TYPE_SUBMITTER,
     TRK_FIELD_TYPE_OWNER, TRK_FIELD_TYPE_USER, TRK_FIELD_TYPE_DATE];
end;

function TPVCSTracker.getFieldNameNumeric(const FieldName: AnsiString): cardinal;
var i: integer;
begin
  i := Fields.IndexOf(FieldName);
  if i<0 then
    result := 0 else
    result := getFieldIndexNumeric(i);
end;

function TPVCSTracker.getFieldNameString(const FieldName: AnsiString): AnsiString;
var i: integer;
begin
  i := Fields.IndexOf(FieldName);
  if i<0 then
    result := '' else
    result := getFieldIndexString(i);
end;

function TPVCSTracker.getFieldIndexNumeric(FieldIndex: integer): cardinal;
begin
  if FieldIsString(FieldIndex) then begin
    Check(fGetStringFieldValue(fRecHandle, PAnsiChar(Fields[FieldIndex]),
      sizeof(tmp), tmp));
    result := StrToIntDef(tmp,0);
  end else
    Check(fGetNumericFieldValue(fRecHandle, PAnsiChar(Fields[FieldIndex]), result));
end;

procedure TPVCSTracker.GetFields(Fields: TStrings; aRecordType: TRK_RECORD_TYPE=1);
// Objects[]=pFieldType
var pFieldType: TRK_FIELD_TYPE;
begin
  Check(fInitFieldList(fHandle,aRecordType));
  Fields.Clear;
  while fGetNextField(fHandle,sizeof(tmp),tmp,pFieldType)=TRK_SUCCESS do
    Fields.AddObject(tmp,pointer(pFieldType));
end;

function TPVCSTracker.getFieldIndexString(FieldIndex: integer): AnsiString;
var num: cardinal;
begin
  if FieldIsString(FieldIndex) then begin
    Check(fGetStringFieldValue(fRecHandle, pointer(Fields[FieldIndex]),
      sizeof(tmp), tmp));
    result := tmp;
  end else begin
    Check(fGetNumericFieldValue(fRecHandle, pointer(Fields[FieldIndex]), num));
    result := IntToStr(num);
  end;
end;

function TPVCSTracker.GetIniFile: AnsiString;
begin
  if fHandle=0 then
    result := '' else begin
    Check(fGetIniFile(fHandle,sizeof(tmp),tmp));
    result := tmp;
  end;
end;

procedure TPVCSTracker.GetLoginInformation(out UserName, ProjectName,
  DBMSType, DBMSName: AnsiString);
begin
  Check(fGetLoginUserName(fHandle,sizeof(tmp),tmp));
  UserName := tmp;
  Check(fGetLoginProjectName(fHandle,sizeof(tmp),tmp));
  ProjectName := tmp;
  Check(fGetLoginDBMSType(fHandle,sizeof(tmp),tmp));
  DBMSType := tmp;
  Check(fGetLoginDBMSName(fHandle,sizeof(tmp),tmp));
  DBMSName := tmp;
end;

function TPVCSTracker.getNumericValue(AttributeID: cardinal): cardinal;
begin
  Check(fGetNumericAttribute(fHandle,AttributeID,result));
end;

function TPVCSTracker.GetProjects(Projects: TStrings): integer;
begin
  result := -1; // return logged project index, -1 if none
  Projects.Clear;
  if not Logged then exit;
  Check(fInitProjectList(fHandle,nil,nil,nil,nil,0));
  while fGetNextProject(fHandle,sizeof(tmp),tmp)=TRK_SUCCESS do
    Projects.Add(tmp);
  Check(fGetLoginProjectName(fHandle,sizeof(tmp),tmp));
  result := Projects.IndexOf(tmp);
end;

procedure TPVCSTracker.GetQueryNames(QueryNames: TStrings);
// Objects[]=pointer(pRecordType)
var pRecordType: TRK_RECORD_TYPE;
begin
  Check(fInitQueryNameList(fHandle));
  QueryNames.Clear;
  while fGetNextQueryName(fHandle,sizeof(tmp),tmp,pRecordType)=TRK_SUCCESS do
    QueryNames.AddObject(tmp,pointer(pRecordType));
end;

function TPVCSTracker.GetRecordTypeName(recordType: TRK_RECORD_TYPE): AnsiString;
begin
  Check(fGetRecordTypeName(fHandle,recordType,sizeof(tmp),tmp));
end;

procedure TPVCSTracker.GetUserInformation(const UserName: AnsiString;
  out UserFullName, UserEmail: AnsiString);
begin
  Check(fGetUserFullName(fHandle,PAnsiChar(UserName),sizeof(tmp),tmp));
  UserFullName := tmp;
  Check(fGetUserEmail(fHandle,PAnsiChar(UserName),sizeof(tmp),tmp));
  UserEmail := tmp;
end;

procedure TPVCSTracker.GetUsers(Users: TStrings);
begin
  Check(fInitUserList(fHandle));
  Users.Clear;
  while fGetNextUser(fHandle,sizeof(tmp),tmp)=TRK_SUCCESS do
    Users.Add(tmp);
end;

procedure TPVCSTracker.Login(const userName, password, projectName: AnsiString);
begin
  Logout;
  Check(fProjectLogin(fHandle,pointer(userName),pointer(password),
    pointer(projectName),pointer(DBMSType),pointer(DBMSName),
    nil,nil,TRK_USE_INI_FILE_DBMS_LOGIN));
  GetFields(Fields);
  FLogged := true;
end;

procedure TPVCSTracker.Logout;
begin
  if not Logged then
    exit;
  FLogged := false;
  Check(fProjectLogout(fHandle));
end;

function TPVCSTracker.NextRecord: boolean;
begin
  result := fGetNextRecord(fRecHandle)=TRK_SUCCESS;
  if result then
    RecordOpened := true;
end;

procedure TPVCSTracker.RecordDelete;
begin
  Check(fDeleteRecord(fRecHandle));
end;

procedure TPVCSTracker.OpenInBox;
begin
  Check(fInTrayInitRecordList(fRecHandle, 0, 0, nil));
  RecordOpened := true; // force release fAttFileHandle and fNoteHandle
end;

procedure TPVCSTracker.OpenQuery(const aQueryName: AnsiString);
begin
  Check(fQueryInitRecordList(fRecHandle, pointer(aQueryName), 0, nil));
  RecordOpened := true; // force release fAttFileHandle and fNoteHandle
end;

procedure TPVCSTracker.setFieldNameNumeric(const FieldName: AnsiString;
  const Value: cardinal);
begin
  setFieldIndexNumeric(Fields.IndexOf(FieldName),Value);
end;

procedure TPVCSTracker.setFieldNameString(const FieldName, Value: AnsiString);
begin
  setFieldIndexString(Fields.IndexOf(FieldName),Value);
end;

procedure TPVCSTracker.setFieldIndexNumeric(FieldIndex: integer; const Value: cardinal);
begin
  if FieldIsString(FieldIndex) then
    Check(fSetStringFieldValue(fRecHandle, pointer(Fields[FieldIndex]),
      pointer(IntToStr(Value)))) else
    Check(fSetNumericFieldValue(fRecHandle, pointer(Fields[FieldIndex]),Value));
end;

procedure TPVCSTracker.setFieldIndexString(FieldIndex: integer; const Value: AnsiString);
var num: cardinal;
    err: integer;
begin
  if FieldIsString(FieldIndex) then
    Check(fSetStringFieldValue(fRecHandle, pointer(Fields[FieldIndex]),
      PAnsiChar(Value))) else begin
    Val(Value,num,err);
    if err=0 then
      Check(fSetNumericFieldValue(fRecHandle, pointer(Fields[FieldIndex]),
        num));
  end;
end;

procedure TPVCSTracker.setNumericValue(AttributeID: cardinal; const Value: cardinal);
begin
  Check(fSetNumericAttribute(fHandle,AttributeID,Value));
end;

procedure TPVCSTracker.RecordCancel;
begin
  if not RecordOpened or (not FRecordUpdate and not FRecordCreate) then exit;
  RecordOpened := false;
  FRecordUpdate := false;
  FRecordCreate := false;
  fRecordCancelTransaction(fRecHandle); // does not matter if failed
end;

function TPVCSTracker.RecordCommit: TRK_TRANSACTION_ID;
begin
  if RecordOpened then
    if FRecordCreate then
      Check(fNewRecordCommit(fRecHandle,result)) else
    if FRecordUpdate then
      Check(fUpdateRecordCommit(fRecHandle,result));
  FRecordUpdate := false;
  FRecordCreate := false;
  RecordOpened := false;
end;

procedure TPVCSTracker.RecordOpen(aRecordId: cardinal);
begin
  if FRecordUpdate or FRecordCreate then
    RecordCancel;
  Check(fGetSingleRecord(fRecHandle,aRecordId,1));
  FRecordUpdate := false;
  FRecordCreate := false;
  RecordOpened := true;
end;

function TPVCSTracker.RecordCreate: cardinal;
begin
  if FRecordUpdate or FRecordCreate then
    RecordCancel;
  Check(fNewRecordBegin(fRecHandle,1));
  RecordOpened := true;
  FRecordUpdate := false;
  FRecordCreate := true;
  result := FieldNameNumeric['ID'];
end;

procedure TPVCSTracker.RecordUpdate(aRecordId: cardinal;
  withModule, withNote: boolean);
var rc: cardinal;
begin
  RecordOpen(aRecordId);
  if withModule and (fAssocHandle=0) then begin
    Check(fAssociationHandleAlloc(fRecHandle,fAssocHandle));
    Check(fInitAssociationList(fAssocHandle));
  end;
  if withNote and (fNoteHandle=0) then begin
    Check(fNoteHandleAlloc(fRecHandle,fNoteHandle));
    Check(fInitNoteList(fNoteHandle));
  end;
  rc := fUpdateRecordBegin(fRecHandle);
  if rc=TRK_E_RECORD_LOCKED then begin // during debugging, record may be locked
    Check(fRecordCancelTransaction(fRecHandle)); // unlock
    Check(fUpdateRecordBegin(fRecHandle));
  end else
    Check(rc); // another error
  FRecordUpdate := true;
end;

function TPVCSTracker.getRecordDescription: AnsiString;
var L: cardinal;
begin
  if RecordOpened then begin
    Check(fGetDescriptionDataLength(fRecHandle,L));
    SetLength(result,L);
    if L<>0 then
      Check(fGetDescriptionData(fRecHandle,L+1,@result[1],L));
    assert(L=0);
  end else
    result := '';
end;

procedure TPVCSTracker.setRecordDescription(const Value: AnsiString);
begin
  if RecordOpened then
    Check(fSetDescriptionData(fRecHandle,length(Value),PAnsiChar(Value),0));
end;

procedure TPVCSTracker.NoteAdd(const aNoteAuthor, aNoteTitle, aNoteData: AnsiString);
begin
  NoteCreate;
  NoteAuthor := aNoteAuthor;
  NoteTitle := aNoteTitle;
  NoteData := aNoteData;
end;

procedure TPVCSTracker.NoteDelete;
begin
  Check(fDeleteNote(fNoteHandle));
end;

procedure TPVCSTracker.SetRecordOpened(const Value: boolean);
begin
  if not Value and not RecordOpened then // RecordOpened=false with nothing
    exit;
  if RecordOpened then begin
    if FRecordUpdate or FRecordCreate then begin
      fRecordCancelTransaction(fRecHandle); // does not matter if failed
      FRecordUpdate := false; // won't use Record Cancel here -> endless loop
      FRecordCreate := false;
    end;
    if fNoteHandle<>0 then begin
      Check(fNoteHandleFree(fNoteHandle));
      fNoteHandle := 0;
    end;
    if fAttFileHandle<>0 then begin
      Check(fAttachedFileHandleFree(fAttFileHandle));
      fAttFileHandle := 0;
    end;
    if fAssocHandle<>0 then begin
      Check(fAssociationHandleFree(fAssocHandle));
      fAssocHandle := 0;
    end;
  end;
  FRecordOpened := Value;
end;

function TPVCSTracker.NextNote: boolean;
begin
  result := false;
  if not RecordOpened then
    exit;
  if fNoteHandle=0 then begin
    Check(fNoteHandleAlloc(fRecHandle,fNoteHandle));
    Check(fInitNoteList(fNoteHandle));
  end;
  result := fGetNextNote(fNoteHandle)=TRK_SUCCESS;
  if not result then begin
    Check(fNoteHandleFree(fNoteHandle));
    fNoteHandle := 0;
  end;
end;

function TPVCSTracker.getNoteTitle: AnsiString;
begin // fNoteHandle=nil if no record opened -> exception raised
  Check(fGetNoteTitle(fNoteHandle,sizeof(tmp),tmp));
  result := tmp;
end;

procedure TPVCSTracker.setNoteTitle(const Value: AnsiString);
begin
  Check(fSetNoteTitle(fNoteHandle,PAnsiChar(Value)));
end;

function TPVCSTracker.getNoteAuthor: AnsiString;
begin // fNoteHandle=nil if no record opened -> exception raised
  Check(fGetNoteAuthor(fNoteHandle,sizeof(tmp),tmp));
  result := tmp;
end;

function GetActiveTimeBias: integer;
// get diff from GMT in minutes (Paris in winter = -60)
var TimeZoneInformation: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TimeZoneInformation) of
    TIME_ZONE_ID_UNKNOWN  :
       result := TimeZoneInformation.Bias;
    TIME_ZONE_ID_DAYLIGHT :
      result := TimeZoneInformation.Bias + TimeZoneInformation.DaylightBias;
    TIME_ZONE_ID_STANDARD :
      result := TimeZoneInformation.Bias + TimeZoneInformation.StandardBias;
    else
      result := 0;
  end;
end;

function cTimeToLocalDateTime(const cTime: Int64): TDateTime;
// convert a gmt-based c-encoded time (unix-encoded) as local TDateTime
begin
  result := (cTime / SecsPerDay + UnixDateDelta) - ActiveTimeBias;
end;

function TPVCSTracker.getNoteTime: TDateTime;
var cTime: TRK_TIME;
begin
  Check(fGetNoteModifyTime(fNoteHandle,cTime));
  if cTime=0 then
    Check(fGetNoteCreateTime(fNoteHandle,cTime));
  result := cTimeToLocalDateTime(cTime);
//  result := cTime / SecsPerDay + UnixDateDelta; // c time to TDateTime
end;

procedure TPVCSTracker.setNoteTime(const Value: TDateTime);
begin
  Check(fSetNoteModifyTime(fNoteHandle,
    Round((Value - UnixDateDelta) * SecsPerDay))); // TDateTime to c time
end;

function TPVCSTracker.getNoteData: AnsiString;
var L: cardinal;
begin
  Check(fGetNoteDataLength(fNoteHandle,L));
  SetLength(result,L);
  if L<>0 then
    Check(fGetNoteData(fNoteHandle,L+1,@result[1],L));
//  assert(L=0);
end;

procedure TPVCSTracker.setNoteAuthor(const Value: AnsiString);
begin
  Check(fSetNoteAuthor(fNoteHandle,PAnsiChar(Value)));
end;

procedure TPVCSTracker.setNoteData(const Value: AnsiString);
begin
  Check(fSetNoteData(fNoteHandle,length(Value),PAnsiChar(Value),0));
end;

procedure TPVCSTracker.NoteCreate;
begin
  if fNoteHandle=0 then
    Check(fNoteHandleAlloc(fRecHandle,fNoteHandle));
  Check(fAddNewNote(fNoteHandle));
end;

procedure TPVCSTracker.FileAdd(const aFileName: AnsiString);
begin
  if fAttFileHandle=0 then
    Check(fAttachedFileHandleAlloc(fRecHandle,fAttFileHandle));
  Check(fAddNewAttachedFile(fAttFileHandle,PAnsiChar(aFileName),TRK_FILE_BINARY));
end;

procedure TPVCSTracker.FileSaveAs(const aDestFileName: AnsiString);
// If the destination file already exists, it is overwritten
begin
  Check(fExtractAttachedFile(fAttFileHandle,pointer(aDestFileName)));
end;

procedure TPVCSTracker.FileDelete;
begin
  Check(fDeleteAttachedFile(fAttFileHandle));
end;

function TPVCSTracker.NextFile: boolean;
begin
  result := false;
  if not RecordOpened then
    exit;
  if fAttFileHandle=0 then begin
    Check(fAttachedFileHandleAlloc(fRecHandle,fAttFileHandle));
    Check(fInitAttachedFileList(fAttFileHandle));
  end;
  result := fGetNextAttachedFile(fAttFileHandle)=TRK_SUCCESS;
  if not result then begin
    Check(fAttachedFileHandleFree(fAttFileHandle));
    fAttFileHandle := 0;
  end;
end;

function TPVCSTracker.getFileName: AnsiString;
begin
  Check(fGetAttachedFileName(fAttFileHandle,sizeof(tmp),tmp));
  result := tmp;
end;

function TPVCSTracker.getFileTime: TDateTime;
var cTime: TRK_TIME;
begin
  Check(fGetAttachedFileTime(fAttFileHandle,cTime));
  result := cTimeToLocalDateTime(cTime);
//  result := cTime / SecsPerDay + UnixDateDelta; // c time to TDateTime
end;

procedure TPVCSTracker.FillQuery(SCRList: TStrings; const aQueryName: AnsiString);
var ID: integer;
begin
  SCRList.BeginUpdate;
  SCRList.Clear;
  OpenQuery;
  while NextRecord do begin
    ID := FieldNameNumeric['ID'];
    if ID>0 then
      SCRList.AddObject(format('%s (%s)',
        [FieldNameString['Title'],FieldNameString['Status']]),pointer(ID));
  end;
  SCRList.EndUpdate;
end;

function TPVCSTracker.getModuleAuthor: AnsiString;
begin // fAssocHandle=nil if no record opened -> exception raised
  Check(fGetAssociationUser(fAssocHandle,sizeof(tmp),tmp));
  result := tmp;
end;

function TPVCSTracker.getModuleCommit: AnsiString;
begin
  Check(fGetAssociationRevisionFixed(fAssocHandle,sizeof(tmp),tmp));
  result := tmp;
end;

function TPVCSTracker.getModuleFileName: AnsiString;
begin
  Check(fGetAssociationModuleName(fAssocHandle,sizeof(tmp),tmp));
  result := tmp;
end;

function TPVCSTracker.getModuleText: AnsiString;
var L: cardinal;
begin
  Check(fGetAssociationTextLength(fAssocHandle,L));
  SetLength(result,L);
  if L<>0 then
    Check(fGetAssociationText(fAssocHandle,L+1,@result[1],L));
  assert(L=0);
end;

procedure TPVCSTracker.ModuleAdd(const aFileName, aAuthor, aAction, aCommit, aText: AnsiString);
begin
  Check(fAddNewAssociation(fAssocHandle));
  if aAuthor<>'' then
    ModuleAuthor := aAuthor;
  ModuleAction := aAction;
  ModuleCommit := aCommit;
  ModuleText := aText;
end;

procedure TPVCSTracker.ModuleDelete;
begin
  Check(fDeleteAssociation(fAssocHandle));
end;

function TPVCSTracker.NextModule: boolean;
begin
  result := false;
  if not RecordOpened then
    exit;
  if fAssocHandle=0 then begin
    Check(fAssociationHandleAlloc(fRecHandle,fAssocHandle));
    Check(fInitAssociationList(fAssocHandle));
  end;
  result := fGetNextAssociation(fAssocHandle)=TRK_SUCCESS;
  if not result then begin
    Check(fAssociationHandleFree(fAssocHandle));
    fAssocHandle := 0;
  end;
end;

procedure TPVCSTracker.setModuleAuthor(const Value: AnsiString);
begin
  Check(fSetAssociationUser(fAssocHandle,PAnsiChar(Value)));
end;

procedure TPVCSTracker.setModuleCommit(const Value: AnsiString);
begin
  Check(fSetAssociationRevisionFixed(fAssocHandle,PAnsiChar(Value)));
end;

procedure TPVCSTracker.setModuleFileName(const Value: AnsiString);
begin
  Check(fSetAssociationModuleName(fAssocHandle,PAnsiChar(Value)));
end;

procedure TPVCSTracker.setModuleText(const Value: AnsiString);
begin
  Check(fSetAssociationText(fAssocHandle,length(Value),PAnsiChar(Value),0));
end;

function TPVCSTracker.getModuleAction: AnsiString;
begin
  Check(fGetAssociationRevisionFound(fAssocHandle,sizeof(tmp),tmp));
  result := tmp;
end;

procedure TPVCSTracker.setModuleAction(const Value: AnsiString);
begin
  Check(fSetAssociationRevisionFound(fAssocHandle,PAnsiChar(Value)));
end;

initialization
  ActiveTimeBias := GetActiveTimeBias / MinsPerDay;

end.





