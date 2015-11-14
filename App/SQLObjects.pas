unit SQLObjects;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TSQLObject = class;
  TSQLObjects = class;

  TSQLObjectClass = class of TSQLObject;

  TSQLObject = class(TObject)
  private
    FOwner: TSQLObjects;
  public
    constructor Create(AOwner: TSQLObjects);
    destructor Destroy; override;
  end;

  TSQLObjects = class(TObject)
  private
    FOwner: TSQLObject;
    FItems: TObjectList<TSQLObject>;
  public
    constructor Create(AOwner: TSQLObject);
    destructor Destroy; override;

    function GetItem(Index: Integer): TSQLObject;
    function Add(AClass: TSQLObjectClass): TSQLObject;
    procedure Delete(const Index: Integer);
    procedure Clear;
    function Count: Integer;
    property Items[Index: Integer]: TSQLObject read GetItem; default;
  end;

implementation

{ TSQLObject }

constructor TSQLObject.Create(AOwner: TSQLObjects);
begin
  FOwner:= AOwner;

end;

destructor TSQLObject.Destroy;
begin

  inherited;
end;

{ TSQLObjects }

constructor TSQLObjects.Create(AOwner: TSQLObject);
begin
  FOwner:= AOwner;
  FItems:= TObjectList<TSQLObject>.Create(True);
end;

destructor TSQLObjects.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TSQLObjects.Add(AClass: TSQLObjectClass): TSQLObject;
begin
  Result:= TSQLObject(AClass.Create(Self));
  FItems.Add(Result);
end;

procedure TSQLObjects.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TSQLObjects.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TSQLObjects.Delete(const Index: Integer);
begin
  FItems.Delete(Index); //TODO
end;

function TSQLObjects.GetItem(Index: Integer): TSQLObject;
begin
  Result:= FItems[Index];
end;

end.
