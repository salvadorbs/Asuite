{
 * USingleton.pas
 *
 * Provides a base class for singleton objects along with a manager object that
 * records instances of each type of singleton.
 *
 * Based on by code by Yoav Abrahami see <URL:http://bit.ly/cAH0HO>, updated to
 * take advantage of modern Delphi features: generics, class vars, class
 * constructor, class destructor etc.
 *
 * $Rev: 47 $
 * $Date: 2010-05-28 11:33:05 +0000 (Fri, 28 May 2010) $
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is USingleton.pas from SingletonDemo.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK *****
}


unit Kernel.Singleton;

{$MODE Delphi}

interface

type
  TSingleton = class(TObject)
  strict private
    // Frees object instance.
    procedure Dispose;
  strict protected
    // Initialises object. Descendants should override instead of
    // constructor. Does nothing in this base class.
    procedure Initialize; virtual;
    // Tidies up object. Descendants should override this method
    // instead of destructor. Does nothing in this base class.
    procedure Finalize; virtual;
  public
    // Destructor tears down object only if singleton manager permits.
    destructor Destroy; override;
    // Constructor creates new instance only if one does not already exist.
    class function NewInstance: TObject; override;
    // Frees instance data only if singleton manager permits.
    procedure FreeInstance; override;
  end;

{$IFDEF TESTING}
// Frees all singleton instances. Used when testing only.
procedure FreeAllSingletons;
{$ENDIF}

implementation

uses
  SysUtils, Generics.Collections;

type
  TSingletonManager = class(TObject)
  strict private
    // Indicates if manager is destroying singletons
    class var fDestroying: Boolean;
    // Map of class names to singleton instances
    class var fMap: TDictionary<string,TSingleton>;
  {$IFNDEF TESTING}strict{$ENDIF}
  protected
    // Frees all registered singletons.
    class procedure FreeAll;
    // Creates empty singleton class map if it doesn't exist
    class procedure CreateMap;
  public
    // Class constructor. Sets up required class vars.
    class constructor Create;
    // Class destructor. Frees all singletons.
    class destructor Destroy;
    // Register new singleton. Do nothing if already registered.
    class procedure RegisterSingleton(const S: TSingleton);
    // Checks if a singlton of given class name already registered.
    class function SingletonExists(const ClsName: string): Boolean;
    // Look up singelton class name in map. EListError if not found.
    class function Lookup(const ClsName: string): TSingleton;
    // Indicates if the this class is destroying singletons. Singleton
    // instances use this property to allow themselves to be destroyed
    class property Destroying: Boolean read fDestroying write fDestroying;
  end;

{$IFDEF TESTING}
procedure FreeAllSingletons;
begin
  // Can't call class constructor directly so we use following methods.
  // These methods are normally *strict* protected, but relaxed for testing.
  TSingletonManager.FreeAll;
  TSingletonManager.CreateMap;
end;
{$ENDIF}

{ TSingleton }

destructor TSingleton.Destroy;
begin
  // Do not override to tidy up unless you first check
  // TSingletonManager.Destroying and only tidy up if it returns true.
  // Override Finalize instead.
  if TSingletonManager.Destroying then
  begin
    Finalize;
    inherited;
  end;
end;

procedure TSingleton.Dispose;
begin
  inherited FreeInstance;
end;

procedure TSingleton.Finalize;
begin
  // Override this to tidy up the object instead of overriding the destructor
end;

procedure TSingleton.FreeInstance;
begin
  // Check if object can be destroyed
  if TSingletonManager.Destroying then
    inherited;
end;

procedure TSingleton.Initialize;
begin
  // Override to initialise code that would normally be placed in constructor
end;

class function TSingleton.NewInstance: TObject;
var
  S: TSingleton;  // reference to a new singleton
begin
  if not TSingletonManager.SingletonExists(Self.ClassName) then
  begin
    S := TSingleton(inherited NewInstance);
    try
      S.Initialize;
      TSingletonManager.RegisterSingleton(S);
    except
      S.Dispose;
      raise;
    end;
  end;
  Result := TSingletonManager.Lookup(Self.ClassName);
end;

{ TSingletonManager }

class constructor TSingletonManager.Create;
begin
  CreateMap;  // indirection here for testing purposes
end;

class procedure TSingletonManager.CreateMap;
begin
  if not Assigned(fMap) then
    fMap := TDictionary<string,TSingleton>.Create;
end;

class destructor TSingletonManager.Destroy;
begin
  FreeAll;  // indirection here for testing purposes
end;

class procedure TSingletonManager.FreeAll;
var
  SPair: TPair<string, TSingleton>; // classname, singleton instance pair
begin
  // indicate to singletons they can destroy
  Destroying := True;
  // free the singletons in the map, then the map itself
  for SPair in fMap do
    SPair.Value.Free;
  FreeAndNil(fMap);
  Destroying := False;
  // we set fMap = nil and Destroying = False to make it safe to
  // re-create map when testing. Don't bother with this if not testing
end;

class function TSingletonManager.Lookup(const ClsName: string): TSingleton;
begin
  Result := fMap[ClsName];
end;

class procedure TSingletonManager.RegisterSingleton(const S: TSingleton);
begin
  if not SingletonExists(S.ClassName) then
    fMap.Add(S.ClassName, S);
end;

class function TSingletonManager.SingletonExists(
  const ClsName: string): Boolean;
begin
  Result := fMap.ContainsKey(ClsName);
end;

end.

