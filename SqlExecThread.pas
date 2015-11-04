unit SqlExecThread;

(*
  SQL Execution Thread
  (NOT YET IMPLEMENTED)

  The thread class contained in this unit will be responsible for performing
  actual script execution. It will support running a single script on multiple
  database connections at one time, and report back status via events.
*)

interface

uses
  System.Classes, SqlExec, ActiveX, DB, ADODB;

type
  TExecThread = class(TThread)
  private
    FSqlExec: TSqlExec;
    FConn: TADOConnection;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TExecThread }

constructor TExecThread.Create;
begin
  inherited Create(True);
  try

  finally
    Resume;
  end;
end;

destructor TExecThread.Destroy;
begin

  inherited;
end;

procedure TExecThread.Execute;
begin
  CoInitialize(nil);
  try
    FSqlExec:= TSqlExec.Create(nil);
    try
      FConn:= TADOConnection.Create(nil);
      try
        FConn.LoginPrompt:= False;

      finally
        FConn.Free;
      end;
    finally
      FSqlExec.Free;
    end;
  finally
    CoUninitialize;
  end;
end;

end.
