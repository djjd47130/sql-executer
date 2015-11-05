unit uContentBase;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ChromeTabs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Buttons, SynEdit, Vcl.ExtCtrls, System.Actions, Vcl.ActnList;

type
  TContents = class;
  TfrmContentBase = class;



  TContents = class(TObject)
  private
    FItems: TObjectList<TfrmContentBase>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TfrmContentBase = class(TForm)
    Acts: TActionList;
  private
    FOwner: TContents;
  public
    constructor Create(AOwner: TContents); reintroduce; virtual;
    destructor Destroy; override;
  end;

var
  frmContentBase: TfrmContentBase;

implementation

{$R *.dfm}

{ TContents }

constructor TContents.Create;
begin
  FItems:= TObjectList<TfrmContentBase>.Create(True);
end;

destructor TContents.Destroy;
begin
  FItems.Clear; //TODO
  FItems.Free;
  inherited;
end;

{ TfrmContentBase }

constructor TfrmContentBase.Create(AOwner: TContents);
begin
  inherited Create(nil);
  FOwner:= AOwner;
end;

destructor TfrmContentBase.Destroy;
begin

  inherited;
end;

end.
