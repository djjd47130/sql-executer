unit uDatabases;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.CheckLst,
  SQLConnections;

type
  TfrmDatabases = class(TForm)
    Lst: TCheckListBox;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    procedure LoadDatabases(AConn: TServerConnection);
    function CheckedCount: Integer;
  end;

var
  frmDatabases: TfrmDatabases;

implementation

{$R *.dfm}

{ TfrmDatabases }

function TfrmDatabases.CheckedCount: Integer;
var
  X: Integer;
begin
  Result:= 0;
  for X := 0 to Lst.Items.Count-1 do begin
    if Lst.Checked[X] then
      Inc(Result);
  end;
end;

procedure TfrmDatabases.LoadDatabases(AConn: TServerConnection);
var
  X: Integer;
  D: TServerDatabase;
begin
  //AConn.RefreshDatabases;
  Lst.Items.BeginUpdate;
  try
    Lst.Items.Clear;
    for X := 0 to AConn.DatabaseCount-1 do begin
      D:= AConn.Databases[X];
      Lst.Items.AddObject(D.Name, D);
      if AConn.SelDatabases.IndexOf(D.Name) >= 0 then begin
        Lst.Checked[Lst.Items.Count-1]:= True;
      end;
    end;
  finally
    Lst.Items.EndUpdate;
  end;
end;

end.
