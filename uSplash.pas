unit uSplash;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.jpeg;

type
  TfrmSplash = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.dfm}

procedure TfrmSplash.FormCreate(Sender: TObject);
var
  X: Integer;
begin
  Show;
  BringToFront;
  Application.ProcessMessages;
  for X := 1 to 50 do begin
    Sleep(15);
    Application.ProcessMessages;
  end;
end;

end.
