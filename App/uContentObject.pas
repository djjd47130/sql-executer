unit uContentObject;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uContentBase, System.Actions,
  Vcl.ActnList, Vcl.Grids;

type
  TfrmContentObject = class(TfrmContentBase)
    Grd: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmContentObject: TfrmContentObject;

implementation

{$R *.dfm}

procedure TfrmContentObject.FormCreate(Sender: TObject);
begin
  inherited;
  Grd.Align:= alClient;

end;

end.
