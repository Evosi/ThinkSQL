unit uEvsSyncObjs;

{$IFDEF FPC} {$MODE DELPHI} {$MODESWITCH advancedrecords}{$H+}
{$ELSE}
  {$IFDEF WIN32} {$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}
{$DEFINE TestSuite}
// an effort to write a small unit of synchronization objects that could be used instead of the build in objects.
// so far I have a ligth wrapper around the system semaphores and a re entrant mutex that needs testing.
// Using as a guide the little book of semaphores http://greenteapress.com/wp/semaphores/

interface
uses
  {$IFDEF FPC}syncobjs, {$ENDIF}
  {$IFDEF WINDOWS} windows, {$ENDIF}
   sysutils;

const
//Windows infinite
   INFINITE = DWORD($FFFFFFFF);     { Infinite timeout }

   { TODO -ojkoz : Add a lightweight event wrapper. }

type
  {$IFNDEF FPC}
  Int32 = Longint;
  UInt32 = LongWord;
  {$ENDIF}
  TTimeUnit=(tuTicks, tuMiliseconds);

  TEvsStopWatch = record
  private
    FFreq : Int64;
    FStart, FStop, FMaxInterval:Int64;
  public
    procedure Start;     //this where everything starts;
    procedure Stop;
    function Elapsed(const aUnit:TTimeUnit):Int64; //how much time has elapsed from the start.
    procedure Reset;
    function Remaining   :Int64; //time remaining before maxinterval is reached.
    property MaxInterval :Int64 read FMaxInterval write FMaxInterval;//Used mainly for my TryXXXX methods where the timeout is calculated from two or more wait calls.
  end;

  { TEvsSemaphore }
  TSemaphoreHandle = {$IFDEF WINDOWS}THandle{$ELSE}Sem_T{$ENDIF};
  //TEventHandle     = {$IFDEF FPC} syncobjs.TEventHandle {$ELSE} THandle {$ENDIF};
  TEventHandle     = THandle;
  TEvsSyncObj = class(TObject)
  public
    procedure Acquire;virtual;abstract;//(aTimeOut :Integer = windows.INFINITE):LongBool;virtual;abstract; //this is the same as tryacquire there is no point in having two calls.
    function TryAcquire(const aTimeOut :DWORD = INFINITE):LongBool;virtual;abstract; //this is the same as tryacquire there is no point in having two calls.
    procedure Release;virtual;abstract;
  end;

  {a simple wrapper around the system semaphore functionality.}
  TEvsSemaphore = class (TEvsSyncObj)
  private
    {Returns the value of the semaphore }
    FHandle :TSemaphoreHandle;
    function TryRelease(const aCount:Integer =1):LongBool;
  public
    //initial value can be anything eg -5 that means the lock is initialized locked and needs to wait for 5 thread to unlock it
    //max value is always possitive.
    constructor Create(InitialValue, MaxValue :Integer);
    destructor Destroy; override;
  { Decreases the value of semphore by one and returns immediately or blocks the caller
    until the value of semaphore is greater than zero.}
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
    procedure Enter;
    procedure Leave;
  {$IFDEF TestSuite}
    property Handle:TEvsSemaphore read FLock;
  {$ENDIF}
  end;

  // light switch implementation.

  { TEvsLightSwitch }

  TEvsLightSwitch = class(TEvsSyncObj)
  private
    FDataAccess :TEvsMutex;
    FSwitch     :TEvsSemaphore;
    FCount      :Int32;
    function GetState: LongBool;
  public
    procedure Acquire; override;
    procedure Release; override;
    function  TryAcquire(const aTimeOut :DWORD=INFINITE) :LongBool; override;

    function WaitFor(const aTimeOut:DWORD=INFINITE):LongBool;//wait for the switch to be turned of.

    constructor Create;
    destructor  Destroy; override;
    procedure TurnOn;
    procedure TurnOff;
    property IsOn :LongBool read GetState;
  end;



  //a multi read single write synchronizer I found on the net
  //somewhere, can't recall the original author, if you recognise
  //the code please inform me.
  TEvsSemaSynchronizer = class(TObject)
  private
    FReaderSem,  FWriterSem    :TSemaphoreHandle;
    FDataAccess, FWriteAccess  :TRTLCriticalSection;
    FActRead,    FReaders,
    FActWrite,   FWriters      :Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartRead;
    procedure StartWrite;
    procedure EndRead;
    procedure EndWrite;
  end;

  // using the light wait semaphore above implement the rwaders writer lock from the book
  // little book of semaphores http://greenteapress.com/wp/semaphores/

  { TEvsMultiReadSingleWriteCynchronizer }

  TEvsMultiReadSingleWriteCynchronizer = class(TEvsSyncObj)
  private
    FDataAccess  :TEvsMutex;
    FWriteAccess :TEvsMutex;
    FTurnStyle   :TEvsMutex;
    FReaders     :Integer;
  public
    constructor Create;
    destructor Destroy; override;
    //those 3 become synonyms to write access for safety reasons.
    procedure Acquire;override; //this is the same as tryacquire there is no point in having two calls.
    function TryAcquire(const aTimeOut :DWORD = INFINITE):LongBool;override; //this is the same as tryacquire there is no point in having two calls.
    procedure Release;override;

    procedure AcquireRead;
    procedure AcquireWrite;

    //timeout is not very accurate at this point, I need to revise the calculations.
    function  TryAcquireRead (const aTimeOut:Integer):Boolean;
    function  TryAcquireWrite(const aTimeOut:Integer):Boolean;


    procedure ReleaseRead;
    procedure ReleaseWrite;
  {$IFDEF TestSuite}
    property ReadCounter :Integer read FReaders;
    //property WriteCounter :Int64 read FReaders;
  {$ENDIF}
  end;

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

  SemInit      :TSemCreate;    //create and initialize a semaphore
  SemDestroy   :TSemProc;      //Destroy a semaphore
  SemWait      :TSemProc;      //acquire access to the semaphore protected resource and decrease its counter or wait.
  SemTryWait   :TSemFunction;  //try to acquire access for a fixed amount of time if access is given return true if it times out return false.
  SemSignal    :TSemFunction;  //inc the counter by what ever aVal holds.

function OSCheck(const aValue:LongBool):LongBool;
begin
{$IFDEF Windows}
  Result := Win32Check(aValue)
{$ELSE}
   unsupported OS
{$ENDIF}
end;

{$REGION ' TEvsStopWatch '}
function TEvsStopWatch.Elapsed(const aUnit:TTimeUnit):Int64;
var
  vTime:Int64;
begin
  Result := 0;
  if FStart>0  then begin
    if FStop <> 0 then Result := FStop - FStart
    else begin
      QueryPerformanceCounter(vTime);
      Result := vTime-FStart;
    end;
    case aUnit of
      tuTicks : ;
      tuMiliseconds : Result := (1000*Result) div FFreq;
    end;
    Result := (1000 * Result) div FFreq;
  end;
end;

function TEvsStopWatch.Remaining: Int64;
begin
  Result := FMaxInterval - Elapsed(tuMiliseconds)
end;

procedure TEvsStopWatch.Reset;
begin
  FStart := 0;
  FStop  := 0;
  QueryPerformanceFrequency(FFreq);
  FMaxInterval := 0;
end;

procedure TEvsStopWatch.Start;
begin
  QueryPerformanceFrequency(FFreq);
  QueryPerformanceCounter(FStart);
  FStop := 0;
end;

procedure TEvsStopWatch.Stop;
begin
  QueryPerformanceCounter(FStop);
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

//function GetTickCount64:UInt64; external 'kernel32' name 'GetTickCount64';


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

{$REGION ' EVENTS '}
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

{$ENDREGION}

{$REGION '  TEvsSemaSynchronizer '}
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

{$REGION '  TEvsSemaphore '}

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
  if not TryRelease(1) then RaiseLastOSError;
end;

function TEvsSemaphore.TryAcquire(const aTimeOut: DWORD=INFINITE): LongBool;
var
  vRes : LongBool;
begin
  vRes   := SemTryWait(FHandle, aTimeOut);
  Result := vRes;
end;

function TEvsSemaphore.TryRelease(const aCount:Integer =1):LongBool;
begin
  Result := SemSignal(FHandle, aCount);
end;

{$ENDREGION}

{$REGION '  TEvsMutex '}

procedure TEvsMutex.Acquire;
begin
//  if FThreadID = GetCurrentThreadId then  //re entry just add to the counter and move on.
//    InterlockedIncrement(FCount)
//  else begin
//    FLock.Acquire; //not a re entry get the lock or wait
//    FThreadID := GetCurrentThreadId; //you have the lock update the active thread ID
//    InterlockedIncrement(FCount) // make sure that the lock is not release while I'm in here.
//  end;
  TryAcquire(INFINITE);
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
var
  vDbg : Integer;
begin //think about it as it is now only the thread that acquired the lock can unlock it
      //is that what we want?
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
  Result := False;
  if FThreadID = GetCurrentThreadId then begin//re entry just add to the counter and return true.
    InterlockedIncrement(FCount);
    Result := True;
  end else begin  // when fthreadId is 0 then no one is inside when <> 0 then someone ese is inside
    if FLock.TryAcquire(aTimeOut) then begin //not a re entry get the lock or wait
      FThreadID := GetCurrentThreadId; //you have the lock update the active thread ID
      InterlockedIncrement(FCount); // make sure that the lock is not release while I'm in here.
      Result := True;
    end;
  end;
end;

{$ENDREGION}

{$REGION '  TEvsMultiReadSingleWriteCynchronizer '}

procedure TEvsMultiReadSingleWriteCynchronizer.Acquire;
begin
  AcquireWrite;
end;

procedure TEvsMultiReadSingleWriteCynchronizer.AcquireRead;
begin
  TryAcquireRead(INFINITE);
end;

procedure TEvsMultiReadSingleWriteCynchronizer.AcquireWrite;
begin
  TryAcquireWrite(INFINITE);
end;

function  TEvsMultiReadSingleWriteCynchronizer.TryAcquireRead(const aTimeOut :Integer) :Boolean;

var
  vTimer :TEvsStopWatch;
  vTime  :Int64;
  procedure StartTimer(var aTimer:TEvsStopWatch; const aaTimeOut:Integer);inline;
  begin
    aTimer.Reset;
    aTimer.MaxInterval := aaTimeOut;
    aTimer.Start;
  end;
  function Remaining(const aTimer:TEvsStopWatch):Int64;inline;
  begin
    if aTimer.MaxInterval = INFINITE then Result := INFINITE else Result := aTimer.Remaining;
  end;

begin
  StartTimer(vTimer,aTimeOut);
  Result := FTurnStyle.TryAcquire(aTimeOut);
  if Result then begin
    FTurnStyle.Release;
    FDataAccess.Acquire;  //this has to be
    try
      vTime := Remaining(vTimer);
      if vTime <= 0 then begin //if no wait time available then fail and exit.
        Result := False;
        Exit;
      end;
      if InterLockedIncrement(FReaders) = 1 then begin //this is the first reader in
        Result := FWriteAccess.TryAcquire(vTime);
        if not Result then InterLockedDecrement(FReaders);
      end;
    finally
      FDataAccess.Release;
    end;
  end;
end;

function  TEvsMultiReadSingleWriteCynchronizer.TryAcquireWrite(const aTimeOut :Integer) :Boolean;
var
  vTimer : TEvsStopWatch;
  vTime  : Int64;
begin
  vTimer.Reset;
  vTimer.MaxInterval := aTimeOut;
  vTimer.Start;
  Result := FTurnStyle.TryAcquire(aTimeOut);
  if aTimeOut = INFINITE then vTime := aTimeOut else vTime := vTimer.Remaining;
  if vTime <=0 then begin
    if Result then FTurnStyle.Release;
    Result := False;
    Exit;
  end else if Result then Result := FWriteAccess.TryAcquire(vTime)
end;

constructor TEvsMultiReadSingleWriteCynchronizer.Create;
begin
  FDataAccess  := TEvsMutex.Create;
  FWriteAccess := TEvsMutex.Create;
  FTurnStyle   := TEvsMutex.Create;
  inherited;
end;

destructor  TEvsMultiReadSingleWriteCynchronizer.Destroy;
begin
  FDataAccess.Free;
  FWriteAccess.Free;
  FTurnStyle.Free;
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

function  TEvsMultiReadSingleWriteCynchronizer.TryAcquire(const aTimeOut: DWORD): LongBool;
begin
  Result := TryAcquireWrite(aTimeOut);
end;

{$ENDREGION}

{$REGION '  TEvsLightSwitch '}

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

procedure TEvsLightSwitch.Acquire;
begin
  TryAcquire(INFINITE);
end;

procedure TEvsLightSwitch.Release;
begin
  TurnOff;
end;

function TEvsLightSwitch.TryAcquire(const aTimeOut :DWORD) :LongBool;
begin
  TurnOn;
  Result := True;
end;

function TEvsLightSwitch.WaitFor(const aTimeOut :DWORD=INFINITE) :LongBool;
begin
  //Result := False; //lightswitch is off.
  Result := FSwitch.TryAcquire(aTimeOut);
  if Result then Release;
end;

procedure TEvsLightSwitch.TurnOff;
begin
  //FDataAccess.Acquire;
  //try
  //  //Dec(FCount);
  //  //if FCount = 0 then FSwitch.Release;
    if InterLockedDecrement(FCount) = 0 then FSwitch.Release;
  //finally
  //  FDataAccess.Release;
  //end;
end;

procedure TEvsLightSwitch.TurnOn;
begin
  //FDataAccess.Acquire;
  //try
    if InterLockedIncrement(FCount) = 1 then FSwitch.Acquire;
  //  //Inc(FCount);
  //  //if FCount = 1 then FSwitch.Acquire;
  //finally
  //  FDataAccess.Release;
  //end;
end;
{$ENDREGION}

initialization

{$IFDEF WINDOWS}
  InitCS     := Windows.InitializeCriticalSection;
  DeleteCS   := Windows.DeleteCriticalSection;
  EnterCS    := Windows.EnterCriticalSection;
  LeaveCS    := Windows.LeaveCriticalSection;
  TryEnterCS := @Windows.TryEnterCriticalSection;

  EventCreate  := @Windows.CreateEvent;
  EventDestroy := DestroyEvent;
  EventSet     := Windows.SetEvent;
  EventReset   := Windows.ResetEvent;
  EventWait    := @WaitEvent;
{$ELSE}

{$ENDIF}

  SemInit    := SemaCreate;
  SemDestroy := SemaDestroy;
  SemWait    := SemaWait;
  SemTryWait := @SemaTryWait;
  SemSignal  := @SemaSignal;

end.
