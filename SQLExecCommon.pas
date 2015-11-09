unit SQLExecCommon;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages,
  Vcl.Forms,
  SQLConnections;

const
  MSG_CONNECTION_ADD = WM_USER + 100;
  MSG_CONNECTION_DEL = WM_USER + 101;
  MSG_DATABASE_ADD = WM_USER + 102;
  MSG_DATABASE_DEL = WM_USER + 103;

procedure DoConnectionAdded(AConn: TServerConnection);
procedure DoConnectionDeleted(AConn: TServerConnection);
procedure DoDatabaseAdded(ADatabase: PChar);
procedure DoDatabaseDeleted(ADatabase: PChar);

implementation

uses
  uMain2;

procedure DoConnectionAdded(AConn: TServerConnection);
begin
  SendMessage(frmSqlExec2.Wnd, MSG_CONNECTION_ADD, NativeUInt(AConn), 0);
end;

procedure DoConnectionDeleted(AConn: TServerConnection);
begin
  PostMessage(frmSqlExec2.Wnd, MSG_CONNECTION_DEL, NativeUInt(AConn), 0);
end;

procedure DoDatabaseAdded(ADatabase: PChar);
begin
  SendMessage(frmSqlExec2.Wnd, MSG_DATABASE_ADD, NativeUInt(ADatabase), 0);
end;

procedure DoDatabaseDeleted(ADatabase: PChar);
begin
  SendMessage(frmSqlExec2.Wnd, MSG_DATABASE_DEL, NativeUInt(ADatabase), 0);
end;



end.
