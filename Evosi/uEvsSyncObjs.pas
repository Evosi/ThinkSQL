unit uEvsSyncObjs;

{$IFDEF FPC} {$mode delphi}
{$ELSE}
  {$IFDEF WIN32} {$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}
{$DEFINE TestSuite}
// an effort to write a small unit of synchronization objects that could be used instead of the build in objects.
// so far I have a ligth wrapper around the system semaphores and a re entrant mutex that needs testing.

interface
uses
   {$IFDEF FPC}syncobjs, {$ENDIF}
  {$IFDEF WINDOWS} windows, {$ENDIF}
   sysutils;

const
//Windows infinite
   INFINITE = DWORD($FFFFFFFF);     { Infinite timeout }

type
  { TODO -ojkoz : Add a lightweight event wrapper. }
  { TEvsSemaphore }
  TSemaphoreHandle = {$IFDEF WINDOWS}THandle{$ELSE}Sem_T{$ENDIF};
  //TEventHandle     = {$IFDEF FPC} syncobjs.TEventHandle {$ELSE} THandle {$ENDIF};
  TEventHandle     = THandle;
  {a simple wrapper around the system semaphore functionality.
   some light testing was done on linux a bit more on windows.
   for any problems with the code please contact me JKOZ.}
  TEvsSyncObj = class(TObject)
  public
    procedure Acquire;virtual;abstract;//(aTimeOut :Integer = windows.INFINITE):LongBool;virtual;abstract; //this is the same as tryacquire there is no point in having two calls.
    function TryAcquire(const aTimeOut :DWORD = INFINITE):LongBool;virtual;abstract; //this is the same as tryacquire there is no point in having two calls.
    procedure Release;virtual;abstract;
  end;

  TEvsSemaphore = class (TEvsSyncObj)
  private
    {Returns the value of the semaphore }
    FHandle :TSemaphoreHandle;
    function TryRelease(const aCount:Integer =1):LongBool;
  public
    //initial value can be anything eg -5 that means the lock is initialized locked and needs to wait for 5 thread to unlock it before allowing
    //anything to pass
    // max value is always possitive I see no value in having a max value smaller than 1.
    constructor Create(InitialValue, MaxValue :Integer);
    destructor Destroy; override;
  { Decreases the value of semphore, if it is greater than zero and returns immediately.
    or blocks the caller (if the value of semaphore is zero) until the value of semaphore
    is greater than one, i.e., someone calls Release or signal method on the same object. }
    procedure Acquire;override;
    procedure Wait;
    function TryAcquire(const aTimeOut:DWORD=INFINITE):LongBool;override;
    { Increases the value of semaphore. }
    procedure Release;override;
    procedure Signal;
  {$IFDEF TestSuite}
    property Handle:TSemaphoreHandle read FHandle;
  {$ENDIF}
  end;
  // a simple mutex that supports re entry from the same thread. 
  TEvsMutex = class(TEvsSyncObj)
  private
    FThreadID :THandle;
    FCount    :Integer;
    FLock     :TEvsSemaphore;
  public
    constructor Create;
    destructor  Destroy;override;
    procedure Acquire;override;
    procedure Release;override;
    function  TryAcquire(const aTimeOut:DWORD = INFINITE):LongBool;override;
    pRocedure Enter;
    procedure Leave;
  end;

  TEvsSemaSynchronizer = class(TObject)
  private
    FReaderSem,  FWriterSem    : TSemaphoreHandle;
    FDataAccess, FWriteAccess  : TRTLCriticalSection;
    FActRead,    FReaders,
    FActWrite,   FWriters      : Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartRead;
    procedure StartWrite;
    procedure EndRead;
    procedure EndWrite;
  end;

  // light switch implementation.
  // little book of semaphores http://greenteapress.com/wp/semaphores/
  TEvsLightSwitch = class(TObject)
  private
    FDataAccess :TEvsMutex;
    FSwitch     :TEvsSemaphore;
    FCount      :Int64;
    function GetState: LongBool;
  public
    constructor Create;
    destructor Destroy; override;
    procedure TurnOn;
    procedure TurnOff;
    property IsOn :LongBool read GetState;
  end;

  // using the light wait semaphore above implement the rwaders writer lock from the book
  // little book of semaphores http://greenteapress.com/wp/semaphores/
  TEvsMultiReadSingleWriteCynchronizer = class(TEvsSyncObj)
  private
    FDataAccess  :TEvsMutex;
    FWriteAccess :TEvsMutex;
    FTurnStyle   :TEvsMutex;
    FReaders     :Int64;
  public
    constructor Create;
    destructor Destroy; override;
    //this become synonims to write access in order to provide as safe an implementation as possible.  
    procedure Acquire;override;//(aTimeOut :Integer = windows.INFINITE):LongBool;virtual;abstract; //this is the same as tryacquire there is no point in having two calls.
    function TryAcquire(const aTimeOut :DWORD = INFINITE):LongBool;override; //this is the same as tryacquire there is no point in having two calls.
    procedure Release;override;

    procedure AcquireRead;
    procedure AcquireWrite;
    procedure ReleaseRead;
    procedure ReleaseWrite;
  end;

//  TEvsMREWS = class(TEvsMultiReadSingleWriteCynchronizer)
//  end;
  TEvsMREWS = TEvsMultiReadSingleWriteCynchronizer;

implementation

Type
  TCSProcedure    = procedure (var cs : TRTLCriticalSection);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TTryCSFunction  = function  (var cs : TRTLCriticalSection):LongInt;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  TSemFunction = function  (aSemaphore:TSemaphoreHandle; aVal:Longint):LongBool;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TSemCreate   = function  (Start, Max:integer):TSemaphoreHandle;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TSemProc     = procedure (aSem:TSemaphoreHandle);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TSemTryProc  = Procedure (aSem:TSemaphoreHandle; aTimeout:Longint; var aResult:LongBool);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  TEventCreate    = function  (lpEventAttributes: PSecurityAttributes; bManualReset, bInitialState: LongBool; lpName: PAnsiChar): TEventHandle;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TEventFunction  = function  (aEvent: TEventHandle): LongBOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TEventWait      =  function (aEvent: TEventHandle;const aTimeOut:Longint): LongBOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TEventProcedure = Procedure (var aEvent:TEventHandle);

  //function  BasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
  //procedure basiceventdestroy(state:peventstate);
  //procedure basiceventResetEvent(state:peventstate);
  //procedure basiceventSetEvent(state:peventstate);
  //function  basiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;

var
  InitCS       :TCSProcedure;
  DeleteCS     :TCSProcedure;
  EnterCS      :TCSProcedure;
  LeaveCS      :TCSProcedure;
  TryEnterCS   :TTryCSFunction;

  EventCreate  :TEventCreate;
  EventDestroy :TEventFunction;
  EventSet     :TEventFunction;
  EventReset   :TEventFunction;
  EventWait    :TEventWait;

  SemInit      : TSemCreate;        //create and initialize a semaphore
  SemDestroy   : TSemProc;          //Destroy a semaphore
  SemWait      : TSemProc;          //acquire access to the semaphore protected resource and decrease its counter or wait.
  SemTryWait   : TSemFunction;      //try to acquire access for a fixed amount of time if access is given return true if it times out return false.
  SemSignal    : TSemFunction;      //inc the counter by what ever aVal holds.

function OSCheck(const aValue:LongBool):LongBool;
begin
{$IFDEF Windows}

  Result := Win32Check(aValue)
{$ELSE}
   unsupported OS
{$ENDIF}
end;

{$REGION ' TEvsSemaSynchronizer '}
constructor TEvsSemaSynchronizer.Create;
begin
  inherited Create;
  InitCS(FDataAccess);
  InitCS(FWriteAccess);
  FReaderSem := SemInit(0, High(Integer));
  FWriterSem := SemInit(0, High(Integer));
  { Initial values of 0 OK for all counts }
  FActRead  := 0;
  FReaders  := 0;
  FActWrite := 0;
  FWriters  := 0;
end;

destructor TEvsSemaSynchronizer.Destroy;
begin
  DeleteCS(FDataAccess);
  DeleteCS(FWriteAccess);
  SemDestroy(FReaderSem);
  SemDestroy(FWriterSem);
  inherited Destroy;
end;

procedure TEvsSemaSynchronizer.StartRead;
begin
  EnterCS(FDataAccess);
  Inc(FActRead);
  if FActWrite = 0 then begin
    Inc(FReaders);
    SemSignal(FReaderSem, 1);
  end;
  LeaveCS(FDataAccess);
  SemWait(FReaderSem);
end;

procedure TEvsSemaSynchronizer.StartWrite;
begin
  EnterCS(FDataAccess);
  Inc(FActWrite);
  if FReaders = 0 then begin
    Inc(FWriters);
    SemSignal(FWriterSem, 1);
  end;
  LeaveCS(FDataAccess);
  SemWait(FWriterSem);
  EnterCS(FWriteAccess);
end;

procedure TEvsSemaSynchronizer.EndRead;
begin
  EnterCS(FDataAccess);
  Dec(FReaders);
  Dec(FActRead);
  if FReaders = 0 then
  begin
    while FWriters < FActWrite do begin
      Inc(FWriters);
      SemSignal(FWriterSem, 1);
    end;
  end;
  LeaveCS(FDataAccess);
end;

procedure TEvsSemaSynchronizer.EndWrite;
begin
  LeaveCS(FWriteAccess);
  EnterCS(FDataAccess);
  Dec(FWriters);
  Dec(FActWrite);
  if FActWrite = 0 then
  begin
    while FReaders < FActRead do  begin
      Inc(FReaders);
      SemSignal(FReaderSem, 1);
    end;
  end;
  LeaveCS(FDataAccess);
end;

{$ENDREGION}

{$REGION ' TEvsSemaphore '}

constructor TEvsSemaphore.Create(InitialValue,MaxValue:Integer);
begin
  inherited Create;
  FHandle := SemInit(InitialValue, MaxValue);
end;

destructor TEvsSemaphore.Destroy;
begin
  SemDestroy(FHandle);
  inherited Destroy;
end;

procedure TEvsSemaphore.Acquire;
begin
  SemWait(FHandle);
end;

procedure TEvsSemaphore.Wait;
begin
  SemWait(FHandle);
end;

procedure TEvsSemaphore.Release;
begin
  if not TryRelease(1) then RaiseLastOSError;
end;

procedure TEvsSemaphore.Signal;
begin
  if not TryRelease(1) then  RaiseLastOSError; //SemSignal(FHandle, 1);
end;

function TEvsSemaphore.TryAcquire(const aTimeOut: DWORD=INFINITE): LongBool;
begin
  Result := SemTryWait(FHandle, aTimeOut);
end;

function TEvsSemaphore.TryRelease(const aCount:Integer =1):LongBool;
begin
  Result := SemSignal(FHandle, aCount);
end;

{$ENDREGION}

{$REGION ' OS Specific '}

{$REGION ' Unix '}
{$IFDEF UNIX}
procedure SemaInit(aSem:TSemaphoreHandle; Start, Max:integer);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_Init(aSem, InitialValue, MaxValue) <> 0);
end;
function SemCreate(Start, Max:integer):TSemaphoreHandle;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_Init(Result, InitialValue, MaxValue) <> 0);
end;

procedure SemaUp(aSem:TSemaphoreHandle);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_Wait(Handle) = 0);
end;
procedure SemaWait(aSem:TSemaphoreHandle);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_Wait(Handle) = 0);
end;
function SemaWait(aSem:TSemaphoreHandle):Boolean;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  Result := Sem_TryWait(Handle) = 0; //
end;

procedure SemaDown(aSem:TSemaphoreHandle; aCount:Integer =1);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_Wait(Handle) = 0);
end;

procedure SemaSignal(aSem:TSemaphoreHandle; aCount:Integer =1);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_Singal(Handle) = 0);
end;

procedure SemaDestroy(aSem:TSemaphoreHandle);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_Destroy(aSem) = 0);
end;

function SemaGetValue(aSem:TSemaphoreHandle):Integer;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}
begin
  OSCheck(Sem_GetValue(aSem, Result)=0);
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' Windows '}
{$IFDEF Windows}
type
  {$IFDEF FPC}
  ULONG = LongWord;
  PULONG = ^ULONG;
  {$ENDIF}
  NTSTATUS = Longint;

  _SEMAPHORE_INFORMATION_CLASS = (SemaphoreBasicInformation);
  SEMAPHORE_INFORMATION_CLASS  = _SEMAPHORE_INFORMATION_CLASS;
  TSemaphoreInformationClass   = SEMAPHORE_INFORMATION_CLASS;

  _SEMAPHORE_BASIC_INFORMATION = record
    CurrentCount: LongInt;
    MaximumCount: LongInt;
  end;
  SEMAPHORE_BASIC_INFORMATION  = _SEMAPHORE_BASIC_INFORMATION;
  PSEMAPHORE_BASIC_INFORMATION = ^SEMAPHORE_BASIC_INFORMATION;
  TSemaphoreBasicInformation   = SEMAPHORE_BASIC_INFORMATION;

function  NtQuerySemaphore( SemaphoreHandle : THANDLE; SemaphoreInformationClass : SEMAPHORE_INFORMATION_CLASS; SemaphoreInformation : Pointer;
                            SemaphoreInformationLength : ULONG; ResultLength : PULONG): NTSTATUS; stdcall; external  'ntdll.dll';

//function SemaInit(Start, Max:integer):TSemaphoreHandle;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
//begin
//  Result := CreateSemaphore(nil, Start, Max, nil);
//  OSCheck(Result <> 0);
//end;

function SemaCreate(Start, Max:integer):TSemaphoreHandle;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := CreateSemaphore(nil, Start, Max, nil);
  OSCheck(Result <> 0);
end;

procedure SemaWait(aSem:TSemaphoreHandle);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  OSCheck(WaitForSingleObject(aSem, INFINITE) = WAIT_OBJECT_0);
end;

function SemaTryWait(aSem:TSemaphoreHandle; const aTimeout:Longint):LongBool;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := WaitForSingleObject(aSem, aTimeout) = WAIT_OBJECT_0;
end;

function SemaSignal(aSem:TSemaphoreHandle; aCount:Integer=1):LongBool;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := ReleaseSemaphore(aSem, aCount, nil);
end;

procedure SemaDestroy(aSem:TSemaphoreHandle);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  CloseHandle(aSem);
end;

function QuerySemaphore(aSem:TSemaphoreHandle; aCount:DWORD):Integer;
var
  vSemInfo : TSemaphoreBasicInformation;
  vNo      : DWORD;
  ntStatus : Integer;
  vLen      : Integer;
begin
  Result := -1;
  ntStatus := NtQuerySemaphore(aSem, SemaphoreBasicInformation, @vSemInfo, SizeOf(vSemInfo), @vLen);
  if ntStatus = 0 then Result:= vSemInfo.CurrentCount; 
end;

{$ENDIF}
{$ENDREGION}

{$ENDREGION}


{$Region ' EVENTS '}
function DestroyEvent(aEvent: TEventHandle): LongBOOL; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := OSCheck(CloseHandle(aEvent));
end;
//
function WaitEvent(aEvent:TEventHandle; const aTimeOut:Longint):LongBool;{$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := WaitForSingleObject(aEvent, aTimeOut) = WAIT_OBJECT_0;
end;
{$ENDREGION }

{ TEvsMutex }

procedure TEvsMutex.Acquire;
begin
  if FThreadID = GetCurrentThreadId then  //re entry just add to the counter and move on.
    InterlockedIncrement(FCount)
  else begin
    FLock.Acquire; //not a re entry get the lock or wait
    FThreadID := GetCurrentThreadId; //you have the lock update the active thread ID
    InterlockedIncrement(FCount) // make sure that the lock is not release while I'm in here.
  end;
end;

constructor TEvsMutex.Create;
begin
  FLock := TEvsSemaphore.Create(1,1);
  inherited Create;
end;

destructor TEvsMutex.destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TEvsMutex.Enter;
begin
  Acquire;
end;

procedure TEvsMutex.Leave;
begin
  Release;
end;

procedure TEvsMutex.Release;
begin
  if FThreadID = GetCurrentThreadId then begin
    InterlockedDecrement(FCount);
    if FCount = 0 then begin //only release the lock if its the last entry.
      FThreadID := 0; //clear up this is the final release.
      FLock.Release;
    end;
  end;
end;

function TEvsMutex.TryAcquire(const aTimeOut:DWORD = INFINITE): LongBool;
begin
  if FThreadID = GetCurrentThreadId then begin//re entry just add to the counter and return true.
    InterlockedIncrement(FCount);
    Result := True;
  end else begin  // when fthreadId is 0 then no one is inside when <> 0 then someone ese is inside
    FLock.Acquire; //not a re entry get the lock or wait 
    FThreadID := GetCurrentThreadId; //you have the lock update the active thread ID
    InterlockedIncrement(FCount) // make sure that the lock is not release while I'm in here.
  end;
end;

{ TEvsMultiReadSingleWriteCynchronizer }

procedure TEvsMultiReadSingleWriteCynchronizer.Acquire;
begin
  AcquireWrite;
end;

procedure TEvsMultiReadSingleWriteCynchronizer.AcquireRead;
begin
  FTurnStyle.Acquire;
  FTurnStyle.Release;
  FDataAccess.Acquire;
  try
    Inc(FReaders);
    if FReaders = 1 then FWriteAccess.Acquire;
  finally
    FDataAccess.Release;
  end;
end;

procedure TEvsMultiReadSingleWriteCynchronizer.AcquireWrite;
begin
  FTurnStyle.Acquire;
  FWriteAccess.Acquire;
end;

constructor TEvsMultiReadSingleWriteCynchronizer.Create;
begin
  FDataAccess  := TEvsMutex.Create;
  FWriteAccess := TEvsMutex.Create;
  FTurnStyle   := TEvsMutex.Create;
  inherited;
end;

destructor TEvsMultiReadSingleWriteCynchronizer.Destroy;
begin

  inherited;
end;

procedure TEvsMultiReadSingleWriteCynchronizer.Release;
begin
  ReleaseWrite;
end;

procedure TEvsMultiReadSingleWriteCynchronizer.ReleaseRead;
begin
  FDataAccess.Acquire;
  try
    Dec(FReaders);
    if FReaders = 0 then FWriteAccess.Release;
  finally
    FDataAccess.Release;
  end;
end;

procedure TEvsMultiReadSingleWriteCynchronizer.ReleaseWrite;
begin
  FWriteAccess.Release;
  FTurnStyle.Release;
end;

function TEvsMultiReadSingleWriteCynchronizer.TryAcquire(const aTimeOut: DWORD): LongBool;
begin
  Result := FWriteAccess.TryAcquire(aTimeOut);
end;

{ TEvsLightSwitch }

constructor TEvsLightSwitch.Create;
begin
  FDataAccess := TEvsMutex.Create;
  FSwitch := TEvsSemaphore.Create(1,1);
  inherited;
end;

destructor TEvsLightSwitch.Destroy;
begin
  FDataAccess.Free;
  FSwitch.Free;
  inherited;
end;

function TEvsLightSwitch.GetState: LongBool;
begin
  FDataAccess.Acquire;
  try
    Result := (FCount > 0);
  finally
    FDataAccess.Release;
  end;
end;

procedure TEvsLightSwitch.TurnOff;
begin
  FDataAccess.Acquire;
  try
    Dec(FCount);
    if FCount = 0 then FSwitch.Release;
  finally
    FDataAccess.Release;
  end;
end;

procedure TEvsLightSwitch.TurnOn;
begin
  FDataAccess.Acquire;
  try
    Inc(FCount);
    if FCount = 1 then FSwitch.Acquire;
  finally
    FDataAccess.Release;
  end;
end;

initialization
{$IFDEF WINDOWS}
  InitCS := Windows.InitializeCriticalSection;
  DeleteCS     := Windows.DeleteCriticalSection;
  EnterCS      := Windows.EnterCriticalSection;
  LeaveCS      := Windows.LeaveCriticalSection;
  TryEnterCS   := @Windows.TryEnterCriticalSection;

  EventCreate               := @Windows.CreateEvent;
  EventDestroy              := DestroyEvent;
  EventSet                  := Windows.SetEvent;
  EventReset                := Windows.ResetEvent;
  EventWait                 := @WaitEvent;
{$ELSE}

{$ENDIF}

  SemInit      := SemaCreate;
  //SemGetValue  := SemaGetValue;
  SemDestroy   := SemaDestroy;
  SemWait      := SemaWait;
  SemTryWait   := @SemaTryWait;
  SemSignal    := @SemaSignal;


end.
