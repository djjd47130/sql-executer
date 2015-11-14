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
    function GetItem(Index: Integer): String;
    function GetSelected(Index: Integer): Boolean;
    procedure SetSelected(Index: Integer; const Value: Boolean);
  public
    procedure LoadDatabases(AConn: TServerConnection);
    function CheckedCount: Integer;
    function TotalCount: Integer;
    property Items[Index: Integer]: String read GetItem; default;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
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

function TfrmDatabases.GetItem(Index: Integer): String;
begin
  Result:= Lst.Items[Index];
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
    end;
  finally
    Lst.Items.EndUpdate;
  end;
end;

procedure TfrmDatabases.SetSelected(Index: Integer; const Value: Boolean);
begin
  Lst.Checked[Index]:= Value;
end;

function TfrmDatabases.GetSelected(Index: Integer): Boolean;
begin
  Result:= Lst.Checked[Index];
end;

function TfrmDatabases.TotalCount: Integer;
begin
  Result:= Lst.Items.Count;
end;

end.
