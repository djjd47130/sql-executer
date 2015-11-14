unit SQLConnections;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellApi, Winapi.ShlObj,
  Winapi.ActiveX, Winapi.OleDB,

  System.SysUtils, System.Variants, System.Types, System.UITypes,
  System.Classes, System.Generics.Collections, System.Actions,
  System.Win.Registry, System.Win.ComObj,

  Data.DB, Data.Win.ADODB, Datasnap.DBClient, MidasLib,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StdCtrls,  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ImgList, Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Buttons, Vcl.Grids,
  Vcl.JumpList, Vcl.DBGrids,

  SQLExec;

type
  TTreeData = class;
  TServerConnections = class;
  TServerConnection = class;
  TServerDatabase = class;
  TServerDatabaseTable = class;
  TServerDatabaseStoredProc = class;

  TTreeData = class(TObject)
  private
    FNode: TTreeNode;
  public
    constructor Create(ANode: TTreeNode);
    property Node: TTreeNode read FNode;
  end;

  TServerEvent = procedure(Sender: TObject; Server: TServerConnection) of object;

  TServerConnections = class(TObject)
  private
    FTree: TTreeView;
    FItems: TObjectList<TServerConnection>;
    FOnConnect: TServerEvent;
    FOnDisconnect: TServerEvent;
    function GetItem(Index: Integer): TServerConnection;
  public
    constructor Create(ATree: TTreeView);
    destructor Destroy; override;
    function AddConnection(AConnStr: String): TServerConnection;
    function Count: Integer;
    procedure Clear;
    procedure Delete(const Index: Integer);
    function IndexOf(AConnection: TServerConnection): Integer;
  public
    property Items[Index: Integer]: TServerConnection read GetItem; default;

    property OnConnect: TServerEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TServerEvent read FOnDisconnect write FOnDisconnect;
  end;

  TServerConnection = class(TTreeData)
  private
    FOwner: TServerConnections;
    FConnectionString: TConnectionString;
    FConnection: TADOConnection;
    FDatabases: TObjectList<TServerDatabase>;
    FDatabasesLoaded: Boolean;
    //FSelDatabases: TStringList;
    procedure SetConnectionString(const Value: TConnectionString);
    function GetDatabase(const Index: Integer): TServerDatabase;
  public
    constructor Create(ANode: TTreeNode; AOwner: TServerConnections);
    destructor Destroy; override;
    function NewQuery: TADOQuery;
    property Connection: TADOConnection read FConnection;
    property ConnectionString: TConnectionString read FConnectionString write SetConnectionString;
    procedure RefreshDatabases;
    function DatabaseCount: Integer;
    procedure ChangeDatabase(const DB: String);
    property Databases[const Index: Integer]: TServerDatabase read GetDatabase;
    //function SelDatabases: TStringList;
  end;

  TServerDatabase = class(TTreeData)
  private
    FOwner: TServerConnection;
    FName: String;
    FTableNode: TTreeNode;
    FStoredProcNode: TTreeNode;
    FOptionsNode: TTreeNode;
    FTables: TObjectList<TServerDatabaseTable>;
    FStoredProcs: TObjectList<TServerDatabaseStoredProc>;
    FTablesLoaded: Boolean;
    FStoredProcsLoaded: Boolean;
    function GetStoredProc(const Index: Integer): TServerDatabaseStoredProc;
    function GetTable(const Index: Integer): TServerDatabaseTable;
  public
    constructor Create(ANode: TTreeNode; AOwner: TServerConnection; const AName: String);
    destructor Destroy; override;
    procedure RefreshTables;
    procedure RefreshStoredProcs;
    function TableCount: Integer;
    function StoredProcCount: Integer;
    property Name: String read FName;
    property Tables[const Index: Integer]: TServerDatabaseTable read GetTable;
    property StoredProcs[const Index: Integer]: TServerDatabaseStoredProc read GetStoredProc;
  end;

  TServerDatabaseTable = class(TTreeData)
  private
    FOwner: TServerDatabase;
    FTableName: String;
  public
    constructor Create(AOwner: TServerDatabase; const ATableName: String);
    destructor Destroy; override;
    property TableName: String read FTableName;
  end;

  TServerDatabaseStoredProc = class(TTreeData)
  private
    FOwner: TServerDatabase;
    FProcName: String;
  public
    constructor Create(AOwner: TServerDatabase; const AProcName: String);
    destructor Destroy; override;
    property ProcName: String read FProcName;
  end;

implementation

{ TTreeData }

constructor TTreeData.Create(ANode: TTreeNode);
begin
  FNode:= ANode;
end;

{ TServerConnections }

constructor TServerConnections.Create(ATree: TTreeView);
begin
  FTree:= ATree;
  FItems:= TObjectList<TServerConnection>.Create(True);
end;

destructor TServerConnections.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TServerConnections.GetItem(Index: Integer): TServerConnection;
begin
  Result:= FItems[Index];
end;

function TServerConnections.IndexOf(AConnection: TServerConnection): Integer;
begin
  Result:= FItems.IndexOf(AConnection);
end;

function TServerConnections.AddConnection(
  AConnStr: String): TServerConnection;
var
  N: TTreeNode;
  //SN: TTreeNode;
  //EN: TTreeNode;
  CS: TConnectionString;
  function A(const Owner: TTreeNode; const Text: String; const Img, State: Integer): TTreeNode;
  begin
    Result:= FTree.Items.AddChild(Owner, Text);
    Result.ImageIndex:= Img;
    Result.SelectedIndex:= Img;
    Result.StateIndex:= State;
  end;
begin
  CS:= AConnStr;
  N:= A(nil, CS['Data Source'] + '  (' + CS['User Id'] + ')', 22, 1);
  Result:= TServerConnection.Create(N, Self);
  Result.FConnectionString:= AConnStr;
  Result.FConnection.ConnectionString:= AConnStr;
  FItems.Add(Result);
  N.Data:= Result;
  Result.RefreshDatabases;
  {
  SN:= A(N, 'Tables', 58, 2);
  EN:= A(SN, '', -1, 5);
  SN:= A(N, 'Stored Procedures', 78, 3);
  EN:= A(SN, '', -1, 6);
  SN:= A(N, 'Options', 17, 4);
  EN:= A(SN, '', -1, 7);
  }
  N.Expand(False);
  if Assigned(FOnConnect) then
    FOnConnect(Self, Result);
end;

procedure TServerConnections.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TServerConnections.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TServerConnections.Delete(const Index: Integer);
var
  C: TServerConnection;
begin
  C:= FItems[Index];
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, C);
  C.Connection.Connected:= False;
  FTree.Items.Delete(C.Node);
  FItems.Delete(Index);
end;

{ TServerConnection }

procedure TServerConnection.ChangeDatabase(const DB: String);
begin
  FConnection.Connected:= False;
  FConnectionString['Initial Catalog']:= DB;
  FConnection.ConnectionString:= FConnectionString;
  FConnection.Connected:= True;
end;

constructor TServerConnection.Create(ANode: TTreeNode; AOwner: TServerConnections);
begin
  inherited Create(ANode);
  FOwner:= AOwner;
  FConnection:= TADOConnection.Create(nil);
  FConnection.LoginPrompt:= False;
  FDatabases:= TObjectList<TServerDatabase>.Create(True);
  FDatabasesLoaded:= False;
  //FSelDatabases:= TStringList.Create;
end;

destructor TServerConnection.Destroy;
begin
  //FSelDatabases.Free;
  FConnection.Connected:= False;
  FDatabases.Clear;
  FDatabases.Free;
  FConnection.Free;
  inherited;
end;

function TServerConnection.DatabaseCount: Integer;
begin
  Result:= FDatabases.Count;
end;

function TServerConnection.GetDatabase(const Index: Integer): TServerDatabase;
begin
  Result:= FDatabases[Index];
end;

function TServerConnection.NewQuery: TADOQuery;
begin
  Result:= TADOQuery.Create(nil);
  Result.Connection:= FConnection;
end;

procedure TServerConnection.RefreshDatabases;
var
  Q: TADOQuery;
  N: TTreeNode;
  D: TServerDatabase;
  S: String;
begin
  FDatabases.Clear;
  Node.DeleteChildren;
  Q:= NewQuery;
  try
    Q.SQL.Text:= 'select Name from master.dbo.sysdatabases order by Name';
    Q.Open;
    while not Q.Eof do begin
      S:= Q.FieldByName('Name').AsString;
      N:= FOwner.FTree.Items.AddChild(Node, S);
      N.ImageIndex:= 58;
      N.SelectedIndex:= 58;
      N.StateIndex:= 2;
      D:= TServerDatabase.Create(N, Self, S);
      FDatabases.Add(D);
      N.Data:= D;
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

{
function TServerConnection.SelDatabases: TStringList;
begin
  Result:= FSelDatabases;
end;
}

procedure TServerConnection.SetConnectionString(const Value: TConnectionString);
begin
  FConnection.Connected:= False;
  FConnectionString := String(Value);
  FConnection.ConnectionString:= Value;
  FConnection.Connected:= True;
end;

{ TServerDatabase }

constructor TServerDatabase.Create(ANode: TTreeNode; AOwner: TServerConnection;
  const AName: String);
begin
  inherited Create(ANode);
  FOwner:= AOwner;
  FName:= AName;
  FTables:= TObjectList<TServerDatabaseTable>.Create(True);
  FStoredProcs:= TObjectList<TServerDatabaseStoredProc>.Create(True);
  FTablesLoaded:= False;
  FStoredProcsLoaded:= False;
end;

destructor TServerDatabase.Destroy;
begin
  FTables.Clear;
  FStoredProcs.Clear;
  FStoredProcs.Free;
  FTables.Free;
  inherited;
end;

function TServerDatabase.GetStoredProc(const Index: Integer): TServerDatabaseStoredProc;
begin
  Result:= FStoredProcs[Index];
end;

function TServerDatabase.GetTable(const Index: Integer): TServerDatabaseTable;
begin
  Result:= FTables[Index];
end;

procedure TServerDatabase.RefreshStoredProcs;
var
  Q: TADOQuery;
  P: TServerDatabaseStoredProc;
begin
  FStoredProcs.Clear;
  Q:= FOwner.NewQuery;
  try
    Q.SQL.Text:= 'Use '+FName;
    Q.ExecSQL;
    Q.SQL.Text:= 'select * from SysObjects where xType = ''P''';
    Q.Open;
    while not Q.Eof do begin
      P:= TServerDatabaseStoredProc.Create(Self, Q.FieldByName('Name').AsString);
      FStoredProcs.Add(P);
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
  FStoredProcsLoaded:= True;
end;

procedure TServerDatabase.RefreshTables;
var
  Q: TADOQuery;
  T: TServerDatabaseTable;
begin
  FTables.Clear;
  Q:= FOwner.NewQuery;
  try
    Q.SQL.Text:= 'Use '+FName;
    Q.ExecSQL;
    Q.SQL.Text:= 'select * from SysObjects where xType = ''U''';
    Q.Open;
    while not Q.Eof do begin
      T:= TServerDatabaseTable.Create(Self, Q.FieldByName('Name').AsString);
      FTables.Add(T);
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
  FTablesLoaded:= True;
end;

function TServerDatabase.StoredProcCount: Integer;
begin
  if not FStoredProcsLoaded then
    RefreshStoredProcs;
  Result:= FStoredProcs.Count;
end;

function TServerDatabase.TableCount: Integer;
begin
  if not FTablesLoaded then
    RefreshTables;
  Result:= FTables.Count;
end;

{ TServerDatabaseTable }

constructor TServerDatabaseTable.Create(AOwner: TServerDatabase;
  const ATableName: String);
begin
  FOwner:= AOwner;
  FTableName:= ATableName;
end;

destructor TServerDatabaseTable.Destroy;
begin

  inherited;
end;

{ TServerDatabaseStoredProc }

constructor TServerDatabaseStoredProc.Create(AOwner: TServerDatabase;
  const AProcName: String);
begin
  FOwner:= AOwner;
  FProcName:= AProcName;
end;

destructor TServerDatabaseStoredProc.Destroy;
begin

  inherited;
end;

end.
