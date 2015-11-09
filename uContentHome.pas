unit uContentHome;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uContentBase, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Buttons;

type
  TfrmContentHome = class(TfrmContentBase)
    BitBtn1: TBitBtn;
    actNewScript: TAction;
    procedure actNewScriptExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmContentHome: TfrmContentHome;

implementation

{$R *.dfm}

uses
  uDataModule
  , uMain2
  ;

procedure TfrmContentHome.actNewScriptExecute(Sender: TObject);
begin
  inherited;
  frmSqlExec2.actFileNew.Execute;
end;

end.
