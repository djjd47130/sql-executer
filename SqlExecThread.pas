unit SQLExecThread;

(*
  SQL Execution Thread
  (NOT YET IMPLEMENTED)

  The thread class contained in this unit will be responsible for performing
  actual script execution. It will support running a single script on multiple
  database connections at one time, and report back status via events.
*)

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.SyncObjs,
  SqlExec,
  ActiveX, DB, ADODB;

type
  TSQLThreadJob = class;
  TSQLExecThread = class;

  TSQLThreadStatus = (esReady, esBusy, esError);

  TSQLThreadStatusEvent = procedure(Sender: TObject;
    const Status: TSQLThreadStatus) of object;

  TSQLThreadLogEvent = procedure(Sender: TSQLExecThread; const Msg: String) of object;

  TSQLThreadJob = class(TObject)
  private
    FOwner: TSQLExecThread;
    FSQL: TStringList;
    FConnStr: TConnectionString;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    procedure SetConnStr(const Value: TConnectionString);
  public
    constructor Create(AOwner: TSQLExecThread);
    destructor Destroy; override;
    property SQL: TStrings read GetSQL write SetSQL;
    property ConnStr: TConnectionString read FConnStr write SetConnStr;
  end;

  TSQLExecThread = class(TThread)
  private
    FSqlExec: TSqlExec;
    FConn: TADOConnection;
    FQueue: TObjectList<TSQLThreadJob>;
    FStatus: TSQLThreadStatus;
    FErrorMessage: String;
    FCurJob: TSQLThreadJob;
    FLock: TCriticalSection;
    procedure SetStatus(const AStatus: TSQLThreadStatus);
    procedure SetCurJob(const ACurJob: TSQLThreadJob);
    procedure SetErrorMessage(const AErrorMessage: String);
    function CurJob: TSQLThreadJob;
    function NextInQueue: Boolean;
    procedure DoJob;
    procedure DoConnect;
    procedure ClearQueue;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Status: TSQLThreadStatus;
    function ErrorMessage: String;
    procedure AddToQueue(const AJob: TSQLThreadJob);
  end;

implementation

{ TSQLThreadJob }

constructor TSQLThreadJob.Create(AOwner: TSQLExecThread);
begin
  FOwner:= AOwner;
  FSQL:= TStringList.Create;
end;

destructor TSQLThreadJob.Destroy;
begin
  FSQL.Free;
  inherited;
end;

function TSQLThreadJob.GetSQL: TStrings;
begin
  Result:= TStrings(FSQL);
end;

procedure TSQLThreadJob.SetConnStr(const Value: TConnectionString);
begin
  FConnStr := Value;
end;

procedure TSQLThreadJob.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

{ TSQLExecThread }

constructor TSQLExecThread.Create;
begin
  inherited Create(True);
  try
    FLock:= TCriticalSection.Create;
    SetStatus(esBusy);
  finally
    Resume;
  end;
end;

destructor TSQLExecThread.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

procedure TSQLExecThread.AddToQueue(const AJob: TSQLThreadJob);
begin
  FLock.Acquire;
  try
    FQueue.Add(AJob);
  finally
    FLock.Release;
  end;
end;

procedure TSQLExecThread.ClearQueue;
var
  X: Integer;
begin
  FLock.Acquire;
  try
    for X := 0 to FQueue.Count-1 do begin
      FQueue[X].Free;
    end;
    FQueue.Clear;
  finally
    FLock.Release;
  end;
end;

function TSQLExecThread.CurJob: TSQLThreadJob;
begin
  FLock.Acquire;
  try
    Result:= FCurJob;
    //NOTE: This isn't really thread-safe, it's not protecting object contents
    //I don't expect this to be necessary at all though outside of thread
  finally
    FLock.Release;
  end;
end;

procedure TSQLExecThread.SetCurJob(const ACurJob: TSQLThreadJob);
begin
  FLock.Acquire;
  try
    FCurJob:= ACurJob;
  finally
    FLock.Release;
  end;
end;

procedure TSQLExecThread.SetErrorMessage(const AErrorMessage: String);
begin
  FLock.Acquire;
  try
    FErrorMessage:= AErrorMessage;
  finally
    FLock.Release;
  end;
end;

procedure TSQLExecThread.SetStatus(const AStatus: TSQLThreadStatus);
begin
  FLock.Acquire;
  try
    FStatus:= AStatus;
  finally
    FLock.Release;
  end;
end;

function TSQLExecThread.Status: TSQLThreadStatus;
begin
  FLock.Acquire;
  try
    Result:= FStatus;
  finally
    FLock.Release;
  end;
end;

function TSQLExecThread.ErrorMessage: String;
begin
  FLock.Acquire;
  try
    Result:= FErrorMessage;
  finally
    FLock.Release;
  end;
end;

function TSQLExecThread.NextInQueue: Boolean;
begin
  if FQueue.Count > 0 then begin
    SetCurJob(FQueue[0]);
    FQueue.Delete(0);
    Result:= True;
  end else begin
    Result:= False;
  end;
end;

procedure TSQLExecThread.DoConnect;
begin
  if Terminated then Exit;
  FConn.Connected:= False;
  FConn.ConnectionString:= FCurJob.ConnStr;
  try
    FConn.Connected:= True;
    FSqlExec.Connection:= FConn;
  except
    on E: Exception do begin
      //TODO: Replace with custom exception type
      raise Exception.Create('Failed to connect to database: '+E.Message);
    end;
  end;
end;

procedure TSQLExecThread.DoJob;
  procedure HandleSuccess;
  begin
    //TODO: Handle successful execution

  end;
  procedure HandleFailure;
  begin
    //TODO: Handle failed execution

  end;
begin
  if Terminated then Exit;
  try
    // ------- ACTUAL EXECUTION -------
    SetStatus(esBusy);
    try
      FSqlExec.SQL.Assign(FCurJob.SQL);
      case FSqlExec.Execute of
        srSuccess: begin
          HandleSuccess;
        end;
        else begin
          HandleFailure;
        end;
      end;
    finally
      SetStatus(esReady);
    end;
  except
    on E: Exception do begin
      //TODO: Replace with custom exception type
      raise Exception.Create('Failed to execute job: '+E.Message);
    end;
  end;
end;

procedure TSQLExecThread.Execute;
begin
  try
    CoInitialize(nil);
    try
      FQueue:= TObjectList<TSQLThreadJob>.Create(False);
      FSqlExec:= TSqlExec.Create(nil);
      try
        FConn:= TADOConnection.Create(nil);
        try
          FConn.LoginPrompt:= False;
          SetStatus(esReady);
          while not Terminated do begin
            if FQueue.Count > 0 then begin
              try
                if NextInQueue then begin
                  DoConnect;
                  DoJob;
                end;
              except
                on E: Exception do begin
                  //TODO
                end;
              end;
            end;
          end;
        finally
          FConn.Free;
        end;
      finally
        ClearQueue;
        FQueue.Free;
        FSqlExec.Free;
      end;
    finally
      CoUninitialize;
    end;
  except
    on E: Exception do begin
      SetStatus(esError);
      SetErrorMessage('Critical SQL thread failure: '+E.Message);
      Terminate;
    end;
  end;
end;

end.
