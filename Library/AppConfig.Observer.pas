unit AppConfig.Observer;

{$MODE DelphiUnicode}

interface

type
  IConfigObserver = interface
    ['{A1B2C3D4-E5F6-47A8-9B0C-1234567890AB}']
    procedure ConfigChanged(const PropertyName: string = '');
  end;

implementation

end.
