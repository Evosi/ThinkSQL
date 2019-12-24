{**** WARNING WARNING WARNING WARNING WARNING WARNING WARNING ****
 **** WARNING WARNING WARNING WARNING WARNING WARNING WARNING ****

                Needs Testing on non windows system

 **** WARNING WARNING WARNING WARNING WARNING WARNING WARNING ****
 **** WARNING WARNING WARNING WARNING WARNING WARNING WARNING ****}

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
  {.$IFDEF FPC}syncobjs, {.$ENDIF}
  {$IFDEF WINDOWS} windows, {$ENDIF}
   sysutils;

const
//Windows infinite
   INFINITE = DWORD($FFFFFFFF);     { Infinite timeout }

   { TODO -oeljo : Add a lightweight event wrapper. }

type
  {$IFNDEF FPC}
  Int32 = Longint;
  UInt32 = LongWord;
  {$ENDIF}
  TTimeUnit=(tuTicks, tuMiliseconds);

  PEvsStopWatch = ^TStopWatch;

  { TStopWatch }

  TStopWatch = record
  private
    FFreq : Int64;
    FStart, FStop, FMaxInterval:Int64;
  public
    procedure Start;     //this where everything starts;
    procedure Stop;
    function Elapsed(const aUnit:TTimeUnit):Int64; //how much time has elapsed from the start.
    procedure Reset;
    function Expired:Boolean;
    function Remaining   :Int64; //time remaining before maxinterval is reached always in ms.
    property MaxInterval :Int64 read FMaxInterval write FMaxInterval;//always in ms. Used mainly for my TryXXXX methods where the timeout is calculated from two or more wait calls.
  end;
  { TSemaphore }
  TSemaphoreHandle = {$IFDEF WINDOWS}THandle{$ELSE}Sem_T{$ENDIF};
  //TEventHandle     = {$IFDEF FPC} syncobjs.TEventHandle {$ELSE} THandle {$ENDIF};
  TEventHandle     = THandle;

  { TSyncObj }

  ISyncObject = interface (IUnknown)
    ['{DD700950-5079-47EE-BA23-B9B02090CDDA}']
    procedure Acquire;                                             {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function TryAcquire(const aTimeOut :DWORD = INFINITE):LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Release;                                             {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Wait;                                                {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function WaitFor(Const aTimeOut:DWORD = INFINITE):LongBool;    {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
  end;

  TSyncObj = class(TObject)
  public
    procedure Acquire;virtual;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};abstract;
    function  TryAcquire(const aTimeOut :DWORD = INFINITE):LongBool;virtual;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};abstract;
    procedure Release;virtual;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};abstract;
    procedure Wait;virtual;                                                 {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function  WaitFor(Const aTimeOut:DWORD = INFINITE):LongBool;virtual;    {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};abstract;
  end;

  {a simple wrapper around the system semaphore functionality.}
  TSemaphore = class (TSyncObj)
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
    procedure Acquire;override;                                            {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    { Increases the value of semaphore. }
    procedure Release;override;                                            {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Wait;override;                                               {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function WaitFor(const aTimeOut:DWORD = INFINITE):LongBool;override;   {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function TryAcquire(const aTimeOut:DWORD = INFINITE):LongBool;override;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Signal;
  {$IFDEF TestSuite}
    property Handle:TSemaphoreHandle read FHandle;
  {$ENDIF}
  end;

  { TExclusiveMutex }
  // a simple mutex no re entry, only the thread that has the lock can unlock it.
  TExclusiveMutex = class(TSyncObj)//none re entrant mutex.
  private
    FLock     :TSemaphore;
    FThreadID :Integer;
  public
    constructor Create;
    destructor  Destroy;override;
    procedure Acquire;override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Release;override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function  WaitFor(const aTimeOut :DWORD =INFINITE) :LongBool; override;  {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function  TryAcquire(const aTimeOut:DWORD = INFINITE):LongBool;override; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};

    procedure Enter;
    procedure Leave;
  end;

  { TMutex }
  // a simple mutex that supports re entry, only the thread that has the lock can unlock it.
  TMutex = class(TSyncObj)//re entrant mutex.
  private
    FThreadID :THandle;
    FCount    :Integer;
    FLock     :TSemaphore;
    function GetCount :Integer;
    function GetThreadID :TThreadID;
  public
    constructor Create;
    destructor  Destroy;override;

    procedure Acquire;override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Release;override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function  TryAcquire(const aTimeOut:DWORD = INFINITE):LongBool;override; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function  WaitFor(const aTimeOut :DWORD =INFINITE) :LongBool; override;  {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Enter;
    procedure Leave;

    property ThreadID  :TThreadID read GetThreadID;
    property LockCount :Integer   read GetCount;

  {$IFDEF TestSuite}
    property Handle:TSemaphore read FLock;
  {$ENDIF}
  end;

  { TEvent }

  TEvent = class(TSyncObj)
  private
    FEvent     :TSemaphore;
    FAutoReset :Boolean;
  public
    procedure Acquire;override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function TryAcquire(const aTimeOut :DWORD = INFINITE):LongBool;override; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Release;override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function WaitFor(const aTimeOut:DWORD = INFINITE):LongBool;override;     {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Wait;override;                                                 {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Signal;
    procedure Reset;
    constructor Create(const aSignaled, aAutoReset:Boolean);
  end;

  { TThreadLocalCounters }
  //for internal use only, unsafe to use outside the library.
  TThreadLocalCounters = class
  private
    FDataAccess :TMutex;
    FLocalStack :Pointer;
    //FEmptyNode  :Pointer;
    FTotal      :Integer;
    FNilNode    :Pointer;
    FEvent      :TEvent;
    function GetSingleThread :Boolean;  // property gettter is called from the outside.
    function GetThTotal :Integer;
    function GetTotal   :Integer;
    function InitNode   :Pointer;
  protected
    procedure AppendNode(aNode:Pointer);
    procedure RemoveNode(aNode:Pointer);
    procedure FreeStack;
    function GetCounter(const aThreadID:LongInt):Pointer;
    function GetThreadCounter(AutoAppend:Boolean=True):Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    function Increase :Integer;
    function Decrease :Integer;
    property Total        :Integer read GetTotal;
    property ThreadTotal  :Integer read GetThTotal;
    property SingleThread :Boolean read GetSingleThread;//true if it is locked by a single thread only.
  end;

  { TLightSwitch }
  // a light switch is a synchronization object that works like the light switch on a room, the first
  // one in, turns the lights on and the last one out turns them off. It is used to allow access
  // to multiple threads and allow threads that require exclusive access to wait until
  // every one else has finished before entering.
  // This implementation has only the logic behind the switch, it does not care what happens after
  // everyone has exited and the exclusive access thread was signaled. It will happilly accept more
  // visitors entering.
  TLightSwitch = class(TSyncObj)
  private
    FDataAccess :TMutex;
    FSwitch     :TSyncObj;
    FCount      :Int32;
    function GetCount :Integer;
    function GetState: LongBool;
  public
    procedure Acquire; override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Release; override;                                              {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    function  TryAcquire(const aTimeOut :DWORD=INFINITE) :LongBool; override; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    //wait for the switch to be turned of.
    function  WaitFor(const aTimeOut:DWORD=INFINITE):LongBool;override;        {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    procedure Lock;
    procedure Unlock;
    constructor Create(const aLock:TSyncObj);
    destructor  Destroy; override;
    procedure TurnOn(const aLock:TSyncObj = nil);
    procedure TurnOff(const aLock:TSyncObj= nil);
    property IsOn  :LongBool read GetState;
    property Count :Integer  read GetCount;
  end;

  { TThreadLightSwitch }

  // a light switch that keeps track on how many times each thread has acquired access.
  // it will be extended to signal a waiting thread that has already unreleased non waiting locks
  // aka a re entrant switch.
  TThreadLightSwitch = class(TSyncObj)
  private
    FDataAccess :TMutex;
    FSwitch     :TSemaphore;
    FCounter    :TThreadLocalCounters;
    FEvent      :TEvent;
    function GetState :LongBool;
  public
    constructor Create;
    destructor  Destroy; override;

    // adds one more item in the list
    procedure Acquire; override;                                             {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    // removes an item from the list
    procedure Release; override;                                             {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    //always succeds to add one more item in the list.
    function  TryAcquire(const aTimeOut :DWORD=INFINITE) :LongBool;override; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    //wait for the list to empty.
    function WaitFor(Const aTimeOut:DWORD = INFINITE):LongBool;override;     {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
    //calls acquire
    procedure TurnOn;
    //calls reelease
    procedure TurnOff;
    //returns if list count > 0
    property IsOn :LongBool read GetState;
  end;

  //a multi read single write synchronizer I found on the net
  //somewhere, can't recall the original author, if you recognise
  //the code please inform me.
  TSemaSynchronizer = class(TObject)
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

  TMultiReadSingleWriteSynchronizer = class(TSyncObj)
  private
    FDataAccess  :TMutex;
    FWriteAccess :TMutex;
    FReadCntr    :TThreadLocalCounters;
    FReaders     :Integer;
    FReEntryRead :Integer;
    FWriters     :Integer;
    FWriterReads :Integer;
    FWriter      :TThreadID;
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
    function  TryAcquireRead (const aTimeOut:DWORD):Boolean;
    function  TryAcquireWrite(const aTimeOut:DWORD):Boolean;


    procedure ReleaseRead;
    procedure ReleaseWrite;
  {$IFDEF TestSuite}
    property ReadCounter :Integer read FReaders;
    //property WriteCounter :Int64 read FReaders;
  {$ENDIF}
  end;

  TMREWS = TMultiReadSingleWriteSynchronizer;

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

  PCountStackNode = ^TCountStackNode;
  TCountStackNode = Packed Record
    Next     :PCountStackNode;
    ThreadID :Integer;
    Value    :Integer;
  end;
  //function  BasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
  //procedure basiceventdestroy(state:peventstate);
  //procedure basiceventResetEvent(state:peventstate);
  //procedure basiceventSetEvent(state:peventstate);
  //function  basiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;
type
  TOwnedSemaphor = class(TSemaphore)
  end;

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

{ TExclusiveMutex }

constructor TExclusiveMutex.Create;
begin
  inherited Create;
  FLock := TSemaphore.Create(1,1);
end;

destructor TExclusiveMutex.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TExclusiveMutex.Acquire;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  FLock.TryAcquire(INFINITE);
  FThreadID := GetCurrentThreadId;
end;

procedure TExclusiveMutex.Release;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  if FThreadID = GetCurrentThreadId then FLock.Release;
end;

function TExclusiveMutex.WaitFor(Const aTimeOut :DWORD) :LongBool; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  Result := FLock.WaitFor(aTimeOut);
end;

function TExclusiveMutex.TryAcquire(const aTimeOut :DWORD) :LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  Result := FLock.TryAcquire(aTimeOut);
  FThreadID := GetCurrentThreadId;
end;

procedure TExclusiveMutex.Enter;
begin
  Acquire;
end;

procedure TExclusiveMutex.Leave;
begin
  Release;
end;

{ TSyncObj }

procedure TSyncObj.Wait;
begin
  WaitFor(INFINITE);
end;

{ TEvent }

procedure TEvent.Signal;
begin
  FEvent.Release;
end;

procedure TEvent.Reset;
begin
  FEvent.TryAcquire(0); //you either acquire the lock or some on else already locked it in any case it is locked and waits for a set call
end;

procedure TEvent.Acquire;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  FEvent.Acquire;
end;

function TEvent.TryAcquire(const aTimeOut :DWORD) :LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  Result := FEvent.TryAcquire(aTimeOut);
end;

procedure TEvent.Release;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  FEvent.Release;
end;

function TEvent.WaitFor(const aTimeOut :DWORD) :LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  Result := FEvent.WaitFor(aTimeOut);
end;

procedure TEvent.Wait;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  if WaitFor(INFINITE) then begin
    if FAutoReset then Reset;
  end;
end;

constructor TEvent.Create(const aSignaled, aAutoReset :Boolean);
begin
  inherited Create;
  if aSignaled then FEvent := TSemaphore.Create(1,1)
  else FEvent := TSemaphore.Create(0,1);
  FAutoReset := aAutoReset;
end;

{ TThreadLightSwitch }

function TThreadLightSwitch.GetState :LongBool;
begin
  Result := FCounter.Total > 0;
end;

procedure TThreadLightSwitch.Acquire; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  TryAcquire(INFINITE);
end;

procedure TThreadLightSwitch.Release; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  FDataAccess.Acquire;
  try
    if FCounter.Decrease = 0 then FSwitch.Release;
  finally
    FDataAccess.Release;
  end;
end;

function TThreadLightSwitch.TryAcquire(const aTimeOut :DWORD) :LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  Result := True; //assume you are not the first.
  FDataAccess.Acquire;
  try
    //if you are the first turn on the lights
    //FCounter.Increase increses the internal counter by one and returns the new sum.
    if FCounter.Increase = 1 then Result := FSwitch.TryAcquire(aTimeOut);
  finally
    FDataAccess.Release;
  end;
end;

constructor TThreadLightSwitch.Create;
begin
  inherited Create;
  FDataAccess := TMutex.Create;
  FSwitch     := TSemaphore.Create(1,1);
  FCounter    :=TThreadLocalCounters.Create;
end;

destructor TThreadLightSwitch.Destroy;
begin
  FDataAccess.Free;
  FSwitch.Free;
  FCounter.Free;
  inherited Destroy;
end;

function TThreadLightSwitch.WaitFor(Const aTimeOut :DWORD) :LongBool; {$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
var
  vTimer : TStopWatch;
begin
  Result := FSwitch.TryAcquire(aTimeOut);
  if Result then FSwitch.Release;
  Exit;
  vTimer.Reset;
  vTimer.MaxInterval := aTimeOut;

  while (FCounter.Total > FCounter.ThreadTotal) and (not vTimer.Expired)do begin
    Result := FSwitch.TryAcquire(1); //pulling what an ugly situation.

  // while FCounter.Total > FCounter.ThreadTotal do
  //  wait for the other threads to leave the room or the time out value was reached.
  //wait for the other threads to leave.
  //  every time a thread releases a lock an event is raised and
  //  waiting threads can decide if they can take control or go back to sleep.
  //This can be accomplished with waitfor multiple objects in windows I have no
  //idea how it is done on other operating systems.
  end;
  Result := Result or (FCounter.Total <= FCounter.ThreadTotal);
  //if not Result then Result := FCounter.ThreadTotal = FCounter.Total;
end;

procedure TThreadLightSwitch.TurnOn;
begin
  Acquire;
end;

procedure TThreadLightSwitch.TurnOff;
begin
  Release;
end;

{ TThreadLocalCounters }

function TThreadLocalCounters.GetThTotal :Integer;
var
  vTLC :PCountStackNode;
begin
  Result := 0;
  FDataAccess.Acquire;
  try
    vTLC   := GetThreadCounter(False);
    if Assigned(vTLC) and (vTLC <> FNilNode) then Result := vTLC^.Value;
  finally
    FDataAccess.Release;
  end;
end;

function TThreadLocalCounters.GetSingleThread :Boolean;
var
  vTLC :PCountStackNode;
begin
  FDataAccess.Acquire;
  try
    Result := False;
    vTLC   := GetCounter(GetCurrentThreadId);
    if Assigned(vTLC) and (vTLC <> FNilNode) then Result := vTLC^.Value >= Total ;
  finally
    FDataAccess.Release;
  end;
end;

function TThreadLocalCounters.GetTotal :Integer;
begin
  InterLockedExchange(Result, FTotal);
end;

function TThreadLocalCounters.InitNode :Pointer;
begin
  //GetMem(Result, SizeOf(TCountStackNode));
  //FillMemory(Result,SizeOf(TCountStackNode),0);
  Result := AllocMem(SizeOf(TCountStackNode)); //allocmem allocates and clear memory
  PCountStackNode(Result)^.Next := PCountStackNode(Result); //create a null node
end;

procedure TThreadLocalCounters.AppendNode(aNode :Pointer);
var
  vNode :PCountStackNode;
begin
  FDataAccess.Acquire;
  try
    if FLocalStack = FNilNode then begin
      FLocalStack := aNode;
      Exit;
    end;
    vNode := FLocalStack;
    repeat
      if vNode^.Next <> FNilNode then vNode := vNode^.Next;
    until vNode^.Next = FNilNode;
    if (vNode <> FNilNode) then vNode^.Next := aNode;// else FLocalStack := aNode;
  finally
    FDataAccess.Release;
  end;
end;

procedure TThreadLocalCounters.RemoveNode(aNode :pointer);
var
  vHeader:PCountStackNode;
begin
  FDataAccess.Acquire;
  try
    vHeader := FLocalStack;
    if vHeader = FNilNode then Exit;
    repeat
      if (vHeader^.Next <> FNilNode) and (vHeader^.Next <> aNode) then vHeader := vHeader^.Next;
    until (vHeader^.Next = aNode) or (vHeader^.Next = FNilNode);
    if (vHeader^.Next = aNode) then begin
      vHeader^.Next := PCountStackNode(aNode)^.Next;
    end;
    Freemem(aNode, SizeOf(TCountStackNode));
  finally
    FDataAccess.Release;
  end;
end;

function TThreadLocalCounters.GetCounter(const aThreadID :LongInt) :Pointer;
begin
  InterLockedExchange(Integer(Result), Integer(FLocalStack));
  if Result <> FNilNode then
    repeat
      if PCountStackNode(Result)^.ThreadID <> aThreadID then //begin
        //if (PCountStackNode(Result)^.ThreadID = 0) and (FEmptyNode = nil) then FEmptyNode := Result; //keep the first empty node you find
        Result := PCountStackNode(Result)^.Next;
      //end;
    until (PCountStackNode(Result)^.ThreadID = aThreadID) or (Result = FNilNode);
  if (Result = FNilNode) then Result := nil;
end;

function TThreadLocalCounters.GetThreadCounter(AutoAppend:Boolean=True):Pointer;
var
  vHead :PCountStackNode;/// absolute FLocalStack;
  vID   :DWORD;
begin
  Result := FNilNode;
  vID := GetCurrentThreadId;
  FDataAccess.Acquire;
  try
    vHead := GetCounter(vID);
    if (vHead = nil) and AutoAppend then begin
//      if FEmptyNode = nil then begin
        vHead := InitNode;
        vHead^.Next := FNilNode;
        AppendNode(vHead);
  //    end else
  //      vHead := FEmptyNode;
      vHead^.ThreadID := vID;
    end;
  finally
    FDataAccess.Release;
  end;
  Result := vHead;
end;

procedure TThreadLocalCounters.FreeStack;
var
  vNode :PCountStackNode;
begin
  //FDataAccess.Acquire;
  //try
    repeat
      vNode := Pointer(InterLockedExchange(Integer(FLocalStack), Integer(PCountStackNode(FLocalStack)^.Next)));
      if (vNode <> FNilNode) then Freemem(vNode, SizeOf(TCountStackNode));
    until vNode = FNilNode;
  //finally
  //  FDataAccess.Release;
  //end;
end;

constructor TThreadLocalCounters.Create;
begin
  //this creates a single list list of integers
  //every time a thread calls Increase two integers are increased
  // a total counter and a thread counter, and the same happens
  // when Decrease is calle but i decreases them instead.

  //this is going to be used in a my re entrant MREWS. the one I have
  //now it does not work.
  inherited Create;
  FNilNode := InitNode;
  FLocalStack := FNilNode;
  FDataAccess := TMutex.Create;
end;

destructor TThreadLocalCounters.Destroy;
var
  vHeader :PCountStackNode;
  vTmp :PCountStackNode;
begin
  FDataAccess.Acquire;
  try
    vHeader := pointer(InterLockedExchange(integer(FLocalStack), Integer(FNilNode)));
    while vHeader<>FNilNode do begin
      vTmp := pointer(InterLockedExchange(integer(vHeader),Integer(vHeader^.Next)));
      if vTmp <> FNilNode then Freemem(vTmp, SizeOf(TCountStackNode));
    end;
    Freemem(FNilNode, SizeOf(TCountStackNode));
  finally
    FDataAccess.Release;
  end;
  FDataAccess.Free;
  FEvent.Free;
  inherited Destroy;
end;

function TThreadLocalCounters.Increase :Integer;
const
  CreateIfNeeded = True;
var
  vThreadCounter : PCountStackNode;
begin
  Result := -1;
  Result := InterLockedIncrement(FTotal); //first update the global lock.
  vThreadCounter := GetThreadCounter(CreateIfNeeded); //now take your time and update the local data.
  //InterLocked not needed, the code is not writen to allow access to local counters outisde the thread.
  //the only thing is doing is checking the threadIDs that they much and selecting its own local data.
  //It does not support removing the thread data from the internal stack yet.
  InterLockedIncrement(vThreadCounter^.Value);
end;

function TThreadLocalCounters.Decrease :Integer;
var
  vThreadCounter : PCountStackNode;
begin
  Result := -1;
  vThreadCounter := GetThreadCounter(False);
  if Assigned(vThreadCounter) then begin
    //this can only be accessed by the thread it self.
    if InterlockedDecrement(vThreadCounter^.Value) = 0 then vThreadCounter^.ThreadID := 0; //re use the node.
  end;
  Result := InterlockedDecrement(FTotal);
end;

{$REGION ' TEvsStopWatch '}
function TStopWatch.Elapsed(const aUnit:TTimeUnit):Int64;
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

function TStopWatch.Remaining: Int64;
begin
  if FMaxInterval = INFINITE then Result := INFINITE
  else Result := FMaxInterval - Elapsed(tuMiliseconds)
end;

procedure TStopWatch.Reset;
begin
  FStart := 0;
  FStop  := 0;
  QueryPerformanceFrequency(FFreq);
  FMaxInterval := 0;
end;

function TStopWatch.Expired :Boolean;
begin
  Result := (FMaxInterval - Elapsed(tuMiliseconds)) <= 0;
end;

procedure TStopWatch.Start;
begin
  QueryPerformanceFrequency(FFreq);
  QueryPerformanceCounter(FStart);
  FStop := 0;
end;

procedure TStopWatch.Stop;
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
constructor TSemaSynchronizer.Create;
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

destructor TSemaSynchronizer.Destroy;
begin
  DeleteCS(FDataAccess);
  DeleteCS(FWriteAccess);
  SemDestroy(FReaderSem);
  SemDestroy(FWriterSem);
  inherited Destroy;
end;

procedure TSemaSynchronizer.StartRead;
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

procedure TSemaSynchronizer.StartWrite;
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

procedure TSemaSynchronizer.EndRead;
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

procedure TSemaSynchronizer.EndWrite;
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

constructor TSemaphore.Create(InitialValue,MaxValue:Integer);
begin
  inherited Create;
  FHandle := SemInit(InitialValue, MaxValue);
end;

destructor TSemaphore.Destroy;
begin
  SemDestroy(FHandle);
  inherited Destroy;
end;

procedure TSemaphore.Acquire;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  SemWait(FHandle);
end;

procedure TSemaphore.Wait;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  SemWait(FHandle);
end;

function TSemaphore.WaitFor(Const aTimeOut :DWORD) :LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  Result := SemaTryWait(FHandle, aTimeOut);
end;

procedure TSemaphore.Release;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  if not TryRelease(1) then RaiseLastOSError;
end;

procedure TSemaphore.Signal;
begin
  if not TryRelease(1) then RaiseLastOSError;
end;

function TSemaphore.TryAcquire(const aTimeOut: DWORD=INFINITE): LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  Result := SemTryWait(FHandle, aTimeOut);
end;

function TSemaphore.TryRelease(const aCount:Integer =1):LongBool;
begin
  Result := SemSignal(FHandle, aCount);
end;

{$ENDREGION}

{$REGION '  TEvsMutex '}

procedure TMutex.Acquire;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin
  TryAcquire(INFINITE);//deadlock in case of re entry.
end;

function TMutex.GetCount :Integer;
var
  vID :TThreadID;
begin
  Result := -1;
  vID := GetCurrentThreadId; //only the thread that has the lock can safely read the count the rest will get an answer of -1
  //interlocked exchange is used to make sure that the local variable is not changed while reading it.
  if InterLockedExchange(PtrInt(FThreadID), FThreadID) = vID then
    Result := FCount;
end;

function TMutex.GetThreadID :TThreadID;
begin
  Result := InterLockedExchange(ptrint(FThreadID), FThreadID);
end;

constructor TMutex.Create;
begin
  FLock := TSemaphore.Create(1,1);
  inherited Create;
end;

destructor TMutex.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TMutex.Enter;
begin
  TryAcquire(INFINITE);//deadlock in case of re entry.
end;

procedure TMutex.Leave;
begin
  Release;
end;

procedure TMutex.Release;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
begin //only the thread that acquired the lock can unlock it.
  if FThreadID = GetCurrentThreadId then begin
    if InterlockedDecrement(FCount) = 0 then begin //only release the lock if its the last entry.
      InterLockedExchange(PtrInt(FThreadID), 0);           //clean up before release.
      FLock.Release;
    end;
  end;
end;

function TMutex.TryAcquire(const aTimeOut:DWORD = INFINITE): LongBool;{$IFDEF Windows}stdcall {$else} cdecl{$EndIf};
var
  vIncoming:DWORD;
begin
  Result := False;
  vIncoming := GetCurrentThreadId;
  // when fthreadId is 0 then no one is inside when <> 0 then someone else is inside
  if  FThreadID = vIncoming then begin//re entry just add to the counter and return true.
    InterlockedIncrement(FCount);
    Result := True;
  end else begin
    if FLock.TryAcquire(aTimeOut) then begin       //not a re entry get the lock or wait
      InterLockedExchange(PtrInt(FThreadID), vIncoming);   //you have the lock update the active thread ID
      InterlockedIncrement(FCount);                //update the re entrance counter.
      Result := True;
    end;
  end;
end;

function TMutex.WaitFor(const aTimeOut :DWORD) :LongBool; stdcall;
begin
  Result := FLock.WaitFor(aTimeOut);
end;

{$ENDREGION}

{$REGION '  TEvsMultiReadSingleWriteCynchronizer '}

procedure TMultiReadSingleWriteSynchronizer.Acquire;
begin
  AcquireWrite;
end;

procedure TMultiReadSingleWriteSynchronizer.AcquireRead;
begin
  TryAcquireRead(INFINITE);
end;

procedure TMultiReadSingleWriteSynchronizer.AcquireWrite;
begin
  TryAcquireWrite(INFINITE);
end;

function  TMultiReadSingleWriteSynchronizer.TryAcquireRead(const aTimeOut :DWORD) :Boolean;

var
  vTimer :TStopWatch;
  vTime  :Int64;
  procedure StartTimer(var aTimer:TStopWatch; const aaTimeOut:Integer);inline;
  begin
    aTimer.Reset;
    aTimer.MaxInterval := aaTimeOut;
    aTimer.Start;
  end;

begin
//1 mutex.wait()
//2 readers += 1
//3 if readers == 1:
//4 roomEmpty.wait() # first in locks
//5 mutex.signal()
//6---------------------------------------here stops this method
//7 # critical section for readers
//8
//9 mutex.wait()
//10 readers -= 1
//11 if readers == 0:
//12 roomEmpty.signal() # last out unlocks
//13 mutex.signal()
  Result := False;
  FDataAccess.Acquire;
  try
    if FReadCntr.Increase = 1 then begin
      Result := FWriteAccess.TryAcquire(aTimeOut);
    end else
      Result := True;
  finally
    FDataAccess.Release;
  end;

end;

function  TMultiReadSingleWriteSynchronizer.TryAcquireWrite(const aTimeOut :DWORD) :Boolean;
var
//  vTimer : TStopWatch;
//  vTime  : Int64;
  vID : TThreadID;
begin
//1 turnstile.wait()
//2 roomEmpty.wait()
//3 # critical section for writers
//4 turnstile.signal()
//5
//6 roomEmpty.signal()
  vID := GetCurrentThreadId;
  Result := FWriteAccess.TryAcquire(aTimeOut);
  InterLockedExchange(PtrInt(FWriter), vID);
end;

constructor TMultiReadSingleWriteSynchronizer.Create;
begin
  FDataAccess  := TMutex.Create;
  FWriteAccess := TMutex.Create;
  FWriter      := 0;
  FReadCntr    := TThreadLocalCounters.Create;
  inherited;
end;

destructor  TMultiReadSingleWriteSynchronizer.Destroy;
begin
  FReadCntr.Free;
  FDataAccess.Free;
  FWriteAccess.Free;
  inherited;
end;

procedure TMultiReadSingleWriteSynchronizer.Release;
begin
  ReleaseWrite;
end;

procedure TMultiReadSingleWriteSynchronizer.ReleaseRead;
begin
//1 turnstile.wait()
//2 turnstile.signal()
//3
//4 readSwitch.lock(roomEmpty)
//5 # critical section for readers
//6 readSwitch.unlock(roomEmpty)
//1 mutex.wait()
//2 readers += 1
//3 if readers == 1:
//4 roomEmpty.wait() # first in locks
//5 mutex.signal()
//6 --------------------------------------- here stops the acquire read method.
//7 # critical section for readers
//8---------------------------------------- here starts this method
//9 mutex.wait()
//10 readers -= 1
//11 if readers == 0:
//12 roomEmpty.signal() # last out unlocks
//13 mutex.signal()
  FDataAccess.Acquire; //serialization is key at this point.
  try
    if InterLockedDecrement(FReaders) = 0 then FWriteAccess.Release;
  finally
    FDataAccess.Release;
  end;
end;

procedure TMultiReadSingleWriteSynchronizer.ReleaseWrite;
begin
//2 roomEmpty.wait()
//  --------------- writeAcquire ends here
//3 # critical section for writers
//  --------------  this method starts here
//6 roomEmpty.signal()
  if FWriteAccess.LockCount = 1 then
    InterLockedExchange(PtrInt(FWriter), 0);
  FWriteAccess.Release;
end;

function  TMultiReadSingleWriteSynchronizer.TryAcquire(const aTimeOut: DWORD): LongBool;
begin
  Result := TryAcquireWrite(aTimeOut);
end;

{$ENDREGION}

{$REGION '  TEvsLightSwitch '}

constructor TLightSwitch.Create(const aLock :TSyncObj);
begin
  FDataAccess := TMutex.Create;
  if Assigned(aLock) then FSwitch := aLock else FSwitch := TOwnedSemaphor.Create(1,1);
  inherited Create;
end;

destructor TLightSwitch.Destroy;
begin
  FDataAccess.Free;
  if FSwitch is TOwnedSemaphor then FSwitch.Free;
  inherited;
end;

function TLightSwitch.GetState: LongBool;
begin
  FDataAccess.Acquire;
  try
    Result := (FCount > 0);
  finally
    FDataAccess.Release;
  end;
end;

function TLightSwitch.GetCount :Integer;
begin
  Result := InterLockedExchange(FCount, FCount);
end;

procedure TLightSwitch.Acquire; stdcall;
begin
  TryAcquire(INFINITE);
end;

procedure TLightSwitch.Release; stdcall;
begin
  //FDataAccess.Acquire;
  //try
    if InterLockedDecrement(FCount) = 0 then FSwitch.Release;
  //finally
  //  FDataAccess.Release;
  //end;
end;

function TLightSwitch.TryAcquire(const aTimeOut :DWORD) :LongBool; stdcall;
begin
  //FDataAccess.Acquire;
  //try
    Result := True;
    if InterLockedIncrement(FCount) = 1 then Result := FSwitch.TryAcquire(aTimeOut);
  //finally
  //  FDataAccess.Release;
  //end;
end;

function TLightSwitch.WaitFor(const aTimeOut :DWORD) :LongBool; stdcall;
begin
  Result := FSwitch.TryAcquire(aTimeOut);
  if Result then FSwitch.Release;//if the acquisition above was ok make sure the switch remains off.
end;

procedure TLightSwitch.Lock;
begin
  FSwitch.Acquire;
end;

procedure TLightSwitch.Unlock;
begin
  FSwitch.Release;
end;

procedure TLightSwitch.TurnOn(const aLock :TSyncObj= nil);
var
  vBackup :TSyncObj;
begin
  if Assigned(aLock) and (aLock<>FSwitch) then vBackup :=TSyncObj(InterLockedExchange(PtrInt(FSwitch), PtrInt(aLock)));
  TryAcquire(INFINITE);
  if Assigned(aLock) and (aLock<>FSwitch) then InterLockedExchange(PtrInt(FSwitch), PtrInt(vBackup));
end;

procedure TLightSwitch.TurnOff(const aLock :TSyncObj= nil);
var
  vBackup:TSyncObj;
begin
  if Assigned(aLock) then vBackup := TSyncObj(InterLockedExchange(PtrInt(FSwitch), PtrInt(aLock)));
  Release;
  if Assigned(aLock) then InterLockedExchange(PtrInt(FSwitch), PtrInt(vBackup));
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
