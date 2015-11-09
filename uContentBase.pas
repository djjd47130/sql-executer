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

  TfrmContentBaseClass = class of TfrmContentBase;

  TContents = class(TObject)
  private
    FItems: TObjectList<TfrmContentBase>;
    procedure Register(AContent: TfrmContentBase);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TfrmContentBase = class(TForm)
    Acts: TActionList;
    actCloseTab: TAction;
    procedure actCloseTabExecute(Sender: TObject);
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

uses
  uDataModule
  , uMain2
  ;

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

procedure TContents.Register(AContent: TfrmContentBase);
begin
  Self.FItems.Add(AContent);
end;

{ TfrmContentBase }

procedure TfrmContentBase.actCloseTabExecute(Sender: TObject);
begin
  //TODO: Close Tab

end;

constructor TfrmContentBase.Create(AOwner: TContents);
begin
  inherited Create(nil);
  FOwner:= AOwner;
  if Assigned(AOwner) then begin
    AOwner.Register(Self);
  end;
end;

destructor TfrmContentBase.Destroy;
begin

  inherited;
end;

end.
