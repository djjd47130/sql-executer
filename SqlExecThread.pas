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

  TSQLThreadStatusEvent = procedure(Sender: TSQLExecThread; const Status: TSQLThreadStatus) of object;

  TSQLThreadJobEvent = procedure(Sender: TSQLExecThread; const Job: TSQLThreadJob) of object;

  TSQLThreadLogEvent = procedure(Sender: TSQLExecThread; const Job: TSQLThreadJob;
    const Msg: String) of object;

  TSQLThreadBlockLogEvent = procedure(Sender: TSQLExecThread; const Job: TSQLThreadJob;
    const Block: TSqlExecBlock; const Msg: String) of object;

  TSQLThreadBlockEvent = procedure(Sender: TSQLExecThread; const Job: TSQLThreadJob;
    const Block: TSqlExecBlock) of object;

  TSQLThreadErrorEvent = procedure(Sender: TSQLExecThread; const E: Exception) of object;

  TSQLThreadDatasetEvent = procedure(Sender: TSQLExecThread; const Job: TSQLThreadJob;
    const Dataset: TDataset) of object;

  TSQLThreadWorkEvent = procedure(Sender: TSQLExecThread; const CurrentBlock,
    TotalBlocks, CurrentJob, TotalJobs: Integer) of object;

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
    FCurJob: TSQLThreadJob;
    FLock: TCriticalSection;
    FTotalBlocks: Integer;
    FCurrentBlock: Integer;
    FTotalJobs: Integer;
    FCurrentJob: Integer;
    FException: Exception;
    FBlock: TSqlExecBlock;
    FMessage: String;
    FOnStatus: TSQLThreadStatusEvent;
    FOnLog: TSQLThreadLogEvent;
    FOnDataset: TSQLThreadDatasetEvent;
    FOnJobStart: TSQLThreadJobEvent;
    FOnJobEnd: TSQLThreadJobEvent;
    FOnError: TSQLThreadErrorEvent;
    FOnBlockStart: TSQLThreadBlockEvent;
    FOnBlockEnd: TSQLThreadBlockEvent;
    FOnBlockMsg: TSQLThreadBlockLogEvent;
    FOnWork: TSQLThreadWorkEvent;
    procedure SetStatus(const AStatus: TSQLThreadStatus);
    procedure SetCurJob(const ACurJob: TSQLThreadJob);
    function CurJob: TSQLThreadJob;
    function NextInQueue: Boolean;
    procedure ClearQueue;
    procedure DoJob;
    procedure DoConnect;
    procedure DoJobEnd;
    procedure DoJobStart;
    procedure DoException;
    procedure BlockFinished(Sender: TSQLExec; Block: TSQLExecBlock);
    procedure BlockStarted(Sender: TSQLExec; Block: TSQLExecBlock);
    procedure CalcBlocks;
    procedure DoBlockMsg;
    procedure DoOnWork;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Status: TSQLThreadStatus;
    function AddToQueue: TSQLThreadJob;
    property OnStatus: TSQLThreadStatusEvent read FOnStatus write FOnStatus;
    property OnLog: TSQLThreadLogEvent read FOnLog write FOnLog;
    property OnError: TSQLThreadErrorEvent read FOnError write FOnError;
    property OnDataset: TSQLThreadDatasetEvent read FOnDataset write FOnDataset;
    property OnJobStart: TSQLThreadJobEvent read FOnJobStart write FOnJobStart;
    property OnJobEnd: TSQLThreadJobEvent read FOnJobEnd write FOnJobEnd;
    property OnBlockStart: TSQLThreadBlockEvent read FOnBlockStart write FOnBlockStart;
    property OnBlockEnd: TSQLThreadBlockEvent read FOnBlockEnd write FOnBlockEnd;
    property OnBlockMsg: TSQLThreadBlockLogEvent read FOnBlockMsg write FOnBlockMsg;
    property OnWork: TSQLThreadWorkEvent read FOnWork write FOnWork;
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
    FQueue:= TObjectList<TSQLThreadJob>.Create(False);
    FStatus:= esBusy;
    FreeOnTerminate:= True;
  finally

  end;
end;

destructor TSQLExecThread.Destroy;
begin
  ClearQueue;
  FQueue.Free;
  FLock.Free;
  inherited;
end;

function TSQLExecThread.AddToQueue: TSQLThreadJob;
begin
  FLock.Acquire;
  try
    Result:= TSQLThreadJob.Create(Self);
    FQueue.Add(Result);
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

procedure TSQLExecThread.SetStatus(const AStatus: TSQLThreadStatus);
begin
  FLock.Acquire;
  try
    FStatus:= AStatus;
    if Assigned(FOnStatus) then
      FOnStatus(Self, AStatus);
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

procedure TSQLExecThread.DoException;
begin
  if Assigned(FOnError) then
    FOnError(Self, FException);
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
    FSqlExec.Connection:= FConn;
    FSqlExec.SQL.Assign(FCurJob.SQL);
    case FSqlExec.Execute of
      srSuccess: begin
        HandleSuccess;
      end;
      else begin
        HandleFailure;
      end;
    end;
  except
    on E: Exception do begin
      //TODO: Replace with custom exception type
      raise Exception.Create('Failed to execute job: '+E.Message);
    end;
  end;
end;

procedure TSQLExecThread.DoJobStart;
begin
  if Assigned(FOnJobStart) then
    FOnJobStart(Self, FCurJob);
end;

procedure TSQLExecThread.DoOnWork;
begin
  if Assigned(FOnWork) then
    FOnWork(Self, FCurrentBlock, FTotalBlocks, FCurrentJob, FTotalJobs);
end;

procedure TSQLExecThread.DoJobEnd;
begin
  if Assigned(FOnJobEnd) then
    FOnJobEnd(Self, FCurJob);
end;

procedure TSQLExecThread.BlockStarted(Sender: TSQLExec; Block: TSQLExecBlock);
begin
  FBlock:= Block;
  Inc(FCurrentBlock);
  //Synchronize(DoOnWork);
end;

procedure TSQLExecThread.BlockFinished(Sender: TSQLExec; Block: TSQLExecBlock);
var
  X: Integer;
begin
  FBlock:= Block;
  case Block.Status of
    seSuccess: begin

    end;
    else begin
      FMessage:= 'Failure on block '+IntToStr(Block.Index);
      Synchronize(DoBlockMsg);
      if Block.Message <> '' then begin
        FMessage:= Block.Message;
        Synchronize(DoBlockMsg);
      end;
    end;
  end;
  for X := 0 to Block.Errors.Count-1 do begin
    FMessage:= Block.Errors[X];
    Synchronize(DoBlockMsg);
  end;
end;

procedure TSQLExecThread.DoBlockMsg;
begin
  if Assigned(FOnBlockMsg) then
    FOnBlockMsg(Self, FCurJob, FBlock, FMessage);
end;

procedure TSQLExecThread.CalcBlocks;
var
  X: Integer;
  E: TSqlExec;
begin
  FTotalBlocks:= 0;
  E:= TSqlExec.Create(nil);
  try
    for X := 0 to FQueue.Count-1 do begin
      if Terminated then Break;
      E.SQL.Assign(FQueue[X].SQL);
      Inc(FTotalBlocks, E.BlockCount);
    end;
  finally
    E.Free;
  end;
end;

procedure TSQLExecThread.Execute;
  procedure PerformJobs;
  begin
    CalcBlocks;
    FCurrentBlock:= 0;
    FCurrentJob:= 0;
    FTotalJobs:= FQueue.Count;
    while FQueue.Count > 0 do begin
      Inc(FCurrentJob);
      try
        if Terminated then Break;
        if NextInQueue then begin
          try
            Synchronize(DoJobStart);
            try
              // ------- EXECUTION -------
              DoConnect;
              DoJob;
            finally
              Synchronize(DoJobEnd);
            end;
          finally
            FreeAndNil(FCurJob);
          end;
        end;
      except
        on E: Exception do begin
          FException:= E;
          Synchronize(DoException);
        end;
      end;
    end;
  end;
begin
  try
    CoInitialize(nil);
    try
      FSqlExec:= TSqlExec.Create(nil);
      FSqlExec.OnBlockStart:= BlockStarted;
      FSqlExec.OnBlockFinish:= BlockFinished;
      //FSqlExec.OnPrint:= BlockPrinted; //TODO
      FConn:= TADOConnection.Create(nil);
      try
        FConn.LoginPrompt:= False;
        SetStatus(esBusy);
        try
          // ------- EXECUTION -------
          PerformJobs;
        finally
          SetStatus(esReady);
        end;
        Terminate; //TEMP ... ?????
      finally
        ClearQueue;
        FConn.Connected:= False;
        FConn.Free;
        FSqlExec.Free;
      end;
    finally
      CoUninitialize;
    end;
  except
    on E: Exception do begin
      FException:= E;
      Synchronize(DoException);
      FStatus:= esError;
      Terminate; //TEMP: TODO
      raise E;
    end;
  end;
end;

end.
