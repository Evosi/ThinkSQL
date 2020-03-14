{
//╔════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗
//║                                        Copyright© 2018 EVOSI® all rights reserved                                  ║
//╠════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣
//║                                                                                                                    ║
//║                        ▄▄▄▄▄▄▄▄▄▄▄  ▄               ▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄                       ║
//║                       ▐░░░░░░░░░░░▌▐░▌             ▐░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌                      ║
//║                       ▐░█▀▀▀▀▀▀▀▀▀  ▐░▌           ▐░▌ ▐░█▀▀▀▀▀▀▀█░▌▐░█▀▀▀▀▀▀▀▀▀  ▀▀▀▀█░█▀▀▀▀                       ║
//║                       ▐░▌            ▐░▌         ▐░▌  ▐░▌       ▐░▌▐░▌               ▐░▌                           ║
//║                       ▐░█▄▄▄▄▄▄▄▄▄    ▐░▌       ▐░▌   ▐░▌       ▐░▌▐░█▄▄▄▄▄▄▄▄▄      ▐░▌                           ║
//║                       ▐░░░░░░░░░░░▌    ▐░▌     ▐░▌    ▐░▌       ▐░▌▐░░░░░░░░░░░▌     ▐░▌                           ║
//║                       ▐░█▀▀▀▀▀▀▀▀▀      ▐░▌   ▐░▌     ▐░▌       ▐░▌ ▀▀▀▀▀▀▀▀▀█░▌     ▐░▌                           ║
//║                       ▐░▌                ▐░▌ ▐░▌      ▐░▌       ▐░▌          ▐░▌     ▐░▌                           ║
//║                       ▐░█▄▄▄▄▄▄▄▄▄        ▐░▐░▌       ▐░█▄▄▄▄▄▄▄█░▌ ▄▄▄▄▄▄▄▄▄█░▌ ▄▄▄▄█░█▄▄▄▄                       ║
//║                       ▐░░░░░░░░░░░▌        ▐░▌        ▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌                      ║
//║                        ▀▀▀▀▀▀▀▀▀▀▀          ▀          ▀▀▀▀▀▀▀▀▀▀▀  ▀▀▀▀▀▀▀▀▀▀▀  ▀▀▀▀▀▀▀▀▀▀▀                       ║
//║                                                                                                                    ║
//╚════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝

 a small unit used to create threads to acquire and release locks from various synchronization objects for testing.
 all rights reserved.
}
unit uEvsThreadTestUtils;
{$IFDEF FPC}
  {$MODE DELPHIUNICODE}{$H+}
{$ELSE}
  {$IFDEF WIN32} {$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  uEvsSyncObjs, {UMREWSDLL,} SysUtils, Classes, Types,
  {$IFDEF Windows} Windows,{$ENDIF}
  uEvsSyncObjs;

Type
   TLockHandle = pointer;

const
  StatusUnknown   = -2;
  StatusFailed    = -1;
  StatusOK        =  0; //>0 acceptable states; <0 invalid or unexpected states; 0 = no state but everything looks ok.
  StatusAcquired  =  1;
  StatusReleased  =  2;
  StatusTimeOut   =  3;
  StatusIncreasing =  4;
  StatusDecreasing =  5;

  cDefaultTimeOut = 10;//ms we only need it for testing, if it takes longer then something is wrong with the code.

//Creates a thread and tries to get access to the mutex if it times out it returns true otherwise False.
function IsMutexLocked(const aMutex:TMutex) :Boolean;
function LockMutex    (const aMutex:TMutex) :TLockHandle;deprecated;// 'use the lockobject instead';   // grabs the lock on a mutex and returns a pointer to the thread that has the lock.
function LockObject   (const aSynchOBj:TObject):TLockHandle; // grabs the lock on a synch object and returns a pointer to the thread that has the lock.
function ReleaseLock  (var aThread:Pointer):Integer;    // the thread releases the mutex and self destructs, after that the pointer becomes invalid.
function LockStatus   (const aObj:Pointer):Integer;

function ReadLock(const aObj:TObject):TLockHandle;
function WriteLock(const aobj:TObject):TLockHandle;

// functions for testing codegear synch objects
//   those should be merged in to one call
//   and the differences should become internal only.
function CG_LockSyncObject(const aObj:TSynchroObject):TLockHandle; // grabs the lock on a mutex and returns a pointer to the thread that has the lock.
function CG_IsLocked      (const aMutex:TSynchroObject):Boolean;

//local thread counter
//creates a thread which increases the aCounter every aInterval milisecons by 1
//after that it goes to sleep until it receives a termination, dec or inc signal again.
function GetCounterLock(const aCounter:TObject; const aAmount, aInterval:DWORD):TLockHandle;
//given an existing counter thread it increases the counter by 1 for a aCounter times at aInterval miliseconds.
procedure IncreaseCounter(const aCounter:TLockHandle; const aAmount, aInterval:DWORD);
//given an existing counter thread it Decrease the counter by 1 for a aCounter times at aInterval miliseconds.
procedure DecreaseCounter(const aLock :TLockHandle; const aAmount, aDelay:DWORD);
procedure WaitFor(aMiliseconds:DWORD);

function GetThreadID (const aLock:TLockHandle):TThreadID;

implementation
const
  cSuspended = True;
type
  TCountAction = ( ca_Inc, ca_Dec, ca_Quit);
  TEvsThreadTester = class;
  EEvsThreadTestException = class(Exception);
  // simple procedure to be passed to a thread for execution. It get the lock object to be tested
  // the time out for the test at hand, the thread that runs the test and it will return its findings
  // in the aResult parameter.So far the only known states of result are 0 no problems, <> 0 problems
  // in execution the number might indicate the problem in the future.
  // this is defined so I can avoid rewritting the same thread over and over again for different
  // synchronization objects and test algorithms now the thread code is simple and writing
  // a single procedure for different test algorithms is easier.
  TThreadProc = procedure (const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
  //used to try and acquire a lock
  TEvsThreadTester = class(TThread)
  private
    FLock     :TObject;       //Lock to try and acquire.
    FResult   :Longint;       //if the acquire was succesfull this becomes true eitherwise false
    FTimeOut  :DWORD;         //how long will wait for the lock, usually 1000ms
    FMutex    :TSemaphore; //when the execution finished this mutex will be released.
    FProc     :TThreadProc;   //which procedure to execute.
    FExecLock :TSemaphore; //a semaphore that is locked to wait with the ability to get awaken from an other thread;
  protected
    procedure Execute;override;
    //execwait should only be called from the thread it self not outside threads.
    procedure ExecWait;
    //execresume is called from outside threads to wake up the thread.
    procedure ExecResume;
    // as long as the deprecated pause is called from inside thread it self it is safe to use.
  public
    constructor Create(CreateSuspended: Boolean; aLock:TObject; aTimeOut:DWORD; aMutex:TSemaphore; aProc:TThreadProc);
    destructor Destroy; override;
    property LastResult :Longint read FResult;
  end;

  { TEvsCounterThread }

  TEvsCounterThread = class(TEvsThreadTester)
  private
    FAction :TCountAction;
    FCount :UInt32;
    FInterval :LongInt;
    procedure SetAction(aValue :TCountAction);
    procedure SetCount(aValue :UInt32);
    procedure SetInterval(aValue :LongInt);
  public
    property Action  : TCountAction read FAction write SetAction;
    //property Interval: LongInt read FInterval write SetInterval;
    property Count   : UInt32 read FCount write SetCount;
  end;

procedure WaitForThreadTimeoutOrAcquisition;inline;
begin
  Sleep(cDefaultTimeOut+20);//wait until the thread acquires the lock or times out.
end;


{$REGION ' EVS Synchronization thread procedures '}
{acquire and wait for the ok to release.
 Create a thread to acquire a lock and sleep until waken up, after that release the lock and finish execution}
procedure AcquireAndWaitToRelease(const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin
  InterlockedExchange(aResult,StatusUnknown);//unknown status.
  if aLock is TSyncObj then begin
    if TSyncObj(aLock).TryAcquire(aTimeOut) then begin
      InterlockedExchange(aResult, StatusAcquired);
      aThread.ExecWait;           // wait for confirmation, someone else will call the threads execresume.   //this code runs from inside the aThread if not something is very very wrong.
      TSyncObj(aLock).Release; // undo any changes.
      InterlockedExchange(aResult, StatusReleased);
    end else aResult := StatusTimeOut;
  end else aResult := StatusFailed;
end;
{acquire and release at once}
procedure AcquireAndRelease(const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin //thread unsafe code do not use outside the testing environment.
  InterlockedExchange(aResult, StatusUnknown); // failed to try.
  if aLock is TSyncObj then begin
    if TSyncObj(aLock).TryAcquire(aTimeOut) then begin
      InterlockedExchange(aResult, StatusAcquired); //everything its ok.
      TSyncObj(aLock).Release;
    end else InterlockedExchange(aResult, StatusTimeOut);
  end;// raise EEvsThreadTestException.Create('unknown lock object type');
end;

procedure Release(const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin //thread unsafe code do not use outside the testing environment.
  InterlockedExchange(aResult, StatusUnknown); // failed to try.
  try
    if aLock is TSyncObj then begin
      TSyncObj(aLock).Release;
      InterlockedExchange(aResult, StatusReleased); //everything its ok.
    end else if aLock is TSynchroObject then begin
      TSynchroObject(aLock).Release;
      InterlockedExchange(aResult, StatusReleased); //everything its ok.
    end
    ;// else InterlockedExchange(aResult, StatusFailed);
  except
    on E:Exception do
      InterlockedExchange(aResult, StatusFailed); // failed to try.
  end;
end;

procedure AcquireRead(const aLock:TObject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin
  //in this case using the deprecated pause/resume would be acceptable as well
  //since they are called from the threads code it self.
  InterlockedExchange(aResult, StatusUnknown); // failed to try.
  try
    if aLock is TMREWS then begin
      if TMREWS(aLock).TryAcquireRead(cDefaultTimeOut) then
        InterlockedExchange(aResult, StatusAcquired) //everything its ok.
      else
        InterlockedExchange(aResult, StatusFailed);
      aThread.ExecWait; //pauses the thread execution and waits for a wake signal.
      if aThread.LastResult = StatusAcquired then TMREWS(aLock).ReleaseRead;
    end else if aLock is TDelphiMREWS then begin
      if TDelphiMREWS(aLock).StartRead{(cDefaultTimeOut)} then
        InterlockedExchange(aResult, StatusAcquired) //everything its ok.
      else
        InterlockedExchange(aResult, StatusFailed);
      aThread.ExecWait; //pauses the thread execution and waits for a wake signal.
      if aThread.LastResult = StatusAcquired then TMREWS(aLock).ReleaseRead;
    end
    ;// else InterlockedExchange(aResult, StatusFailed);
  except
    on E:Exception do InterlockedExchange(aResult, StatusFailed); // failed to try.
  end;
end;

procedure AcquireWrite(const aLock:TObject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin
  //in this case using the deprecated pause/resume would be acceptable as well
  //since they are called from the threads code it self.
  InterlockedExchange(aResult, StatusUnknown);
  try
    if aLock is TMREWS then begin
      if TMREWS(aLock).TryAcquireWrite(cDefaultTimeOut) then
        InterlockedExchange(aResult, StatusAcquired)
      else
        InterlockedExchange(aResult, StatusFailed);
      aThread.ExecWait;
      if aThread.LastResult = StatusAcquired then TMREWS(aLock).ReleaseWrite;
    end else
      InterlockedExchange(aResult, StatusAcquired);
  except
    on E:Exception do InterlockedExchange(aResult, StatusFailed); // failed to try.
  end;
end;

{used on TEvsThreadLocalCounter objects only.
 It is a continues loop in a thread where the end user singals the thread what he wants to do.
 there are 3 actions that can be signaled
  1) Increase the counter.
  2) Decrease the counter.
  3) Complete the thread execution and exit.
 the timeout parameter is used for a delay between increaments this is to give time to other threads to increament as well
 emulating interthread widing.
}
procedure CounterExec(const aLock:TObject; aTimeout:Integer; aThread: TEvsThreadTester; var aResult:LongInt);
var
  vCntr :Integer;
begin
  //in this case using the deprecated pause/resume would be acceptable as well
  //since they are called from the threads code it self.
  repeat
    InterlockedExchange(aResult, StatusUnknown);
    case TEvsCounterThread(aThread).Action of
      ca_Inc : begin
                 InterlockedExchange(aResult, StatusOK);
                 for vCntr := 1 to TEvsCounterThread(aThread).Count do begin
                   TThreadLocalCounters(aLock).Increase;
                   if aTimeout > 0 then Sleep(aTimeout);
                   if TEvsCounterThread(aThread).Action <> ca_Inc then Break;
                 end;
                 InterlockedExchange(aResult, StatusOK);
               end;
      ca_Dec : begin
                 for vCntr := 1 to TEvsCounterThread(aThread).Count do begin
                   TThreadLocalCounters(aLock).Decrease;
                   if aTimeout > 0 then Sleep(aTimeout);
                   if TEvsCounterThread(aThread).Action <> ca_Dec then Break;
                 end;
                 InterlockedExchange(aResult, StatusOK);
               end;
    end;
    if TEvsCounterThread(aThread).Action <> ca_Quit then
      aThread.ExecWait;
  until TEvsCounterThread(aThread).Action = ca_Quit;
  InterLockedExchange(aResult, StatusReleased);
end;

{$ENDREGION}

{$REGION ' CRITICAL SECTION '}
procedure CS_AcquireAndWaitToRelease(const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin
  InterlockedExchange(aResult, StatusUnknown); //atomic aResult := StatusUnknown;
  if aLock is TCriticalSection then begin
    if TCriticalSection(aLock).TryEnter then begin
      InterlockedExchange(aResult, StatusAcquired);
      aThread.ExecWait;
      TCriticalSection(aLock).Leave;
      InterlockedExchange(aResult, StatusReleased);
    end else InterlockedExchange(aResult, StatusTimeOut);
  end else InterlockedExchange(aResult, StatusFailed);
end;
procedure CS_AcquireAndRelease(const aLock:TObject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin //thread unsafe code do not use outside the testing environment.
  InterlockedExchange(aResult, StatusUnknown);
  if aLock is TCriticalSection then begin
    if TCriticalSection(aLock).TryEnter then begin
      InterlockedExchange(aResult, StatusAcquired);
      TCriticalSection(aLock).Leave;
    end else aResult := StatusTimeOut;
  end;
end;
procedure CS_Acquire(const aLock:TObject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin //thread unsafe code do not use outside the testing environment.
  aResult := StatusUnknown; // failed to try.
  if aLock is TCriticalSection then begin
    if TCriticalSection(aLock).TryEnter then begin
      InterlockedExchange(aResult, StatusAcquired);//everything its ok.
    end else aResult := StatusTimeOut;
  end;
end;
procedure CS_Release(const aLock:TObject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin //thread unsafe code do not use outside the testing environment.
  InterlockedExchange(aResult, StatusUnknown); // failed to try.
  try
    if aLock is TCriticalSection then begin
      TCriticalSection(aLock).Release;
      InterlockedExchange(aResult, StatusReleased);//everything its ok.
    end;

  except
    on E:Exception do
      InterlockedExchange(aResult, StatusFailed); // failed to try.
  end;
end;

{ TEvsCounterThread }

procedure TEvsCounterThread.SetAction(aValue :TCountAction);
begin
  if FAction = aValue then Exit;
  FAction := aValue;
end;

procedure TEvsCounterThread.SetCount(aValue :UInt32);
begin
  if FCount =aValue then Exit;
  FCount :=aValue;
end;

procedure TEvsCounterThread.SetInterval(aValue :LongInt);
begin
  if FInterval =aValue then Exit;
  FInterval :=aValue;
end;

{$ENDREGION}

{$REGION ' TEvsThreadTester '}


{-------------------------------------------------------------------------------
  Procedure: TEvsThreadTester.Create
  Author:    JKOZ
  DateTime:  2018.06.01
  Arguments: CreateSuspended: Boolean;        Create the thread suspended or not.
             aLock:TSyncObj;               Object to try and lock
             aTimeOut:DWORD;                  How long it will wait for the lock
             aMutex:TSemaphore             This will be released upon complition of execution. after that the thread can be destroyed.
-------------------------------------------------------------------------------}
constructor TEvsThreadTester.Create(CreateSuspended: Boolean; aLock:TObject; aTimeOut:DWORD; aMutex:TSemaphore; aProc:TThreadProc);
begin
  inherited Create(CreateSuspended);
  Flock     := aLock;
  FTimeOut  := aTimeOut;
  FMutex    := aMutex;
  FProc     := aProc;
  FExecLock := TSemaphore.Create(0,1); //it comes already locked then next thread should wait.
end;

{-------------------------------------------------------------------------------
  Procedure: TEvsThreadTester.Execute
  Author:    JKOZ
  DateTime:  2018.06.01
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
destructor TEvsThreadTester.Destroy;
begin
  FExecLock.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Procedure: TEvsThreadTester.ExecResume;
  Author:    JKOZ
  DateTime:  2018.06.01
  Arguments: None
  Result:    None
             Signals the internal lock to allow the thread code to resume aexecution
-------------------------------------------------------------------------------}
procedure TEvsThreadTester.ExecResume;
begin
  FExecLock.Signal;
end;

{-------------------------------------------------------------------------------
  Procedure:   TEvsThreadTester.Execute;
  Author:      JKOZ
  DateTime:    2018.06.01
  Arguments:   None
  Description: Calls the procedure passed in its constructor and after the procedure
               returns releases the mutex allowing who ever waits on the other end
               to continue.
-------------------------------------------------------------------------------}
procedure TEvsThreadTester.Execute;
begin
  FProc(FLock, FTimeOut, Self, FResult);
  if Assigned(FMutex) then FMutex.Release;//signal we are done
end;

{-------------------------------------------------------------------------------
  Procedure:   TEvsThreadTester.ExecWait;
  Author:      JKOZ
  DateTime:    2018.06.01
  Arguments:   None
  Description: The calling thread is paused and waits for some else to wake it up.
               Be aware it does not pause the thread object is in, so be very carefull
               where you call it from
-------------------------------------------------------------------------------}
procedure TEvsThreadTester.ExecWait;
begin
  FExecLock.Wait;
end;

{$ENDREGION}

{$REGION ' Global section '}

Function LockMutex(const aMutex:TMutex):Pointer;
var
  vThread:TEvsThreadTester;
begin
  Result := nil;
  vThread := TEvsThreadTester.Create(False, aMutex, cDefaultTimeOut, nil, AcquireAndWaitToRelease);
  Sleep(cDefaultTimeOut+10);//wait for the thread to lock or time out.
  Result  := vThread;
end;

function LockObject(const aSynchOBj:TObject):Pointer; // grabs the lock on a mutex and returns a pointer to the thread that has the lock.
var
  vThread:TEvsThreadTester;
begin
  Result := nil; //failed to get a lock;
  if aSynchObj is TSyncObj then begin
    vThread := TEvsThreadTester.Create(False, aSynchOBj, cDefaultTimeOut, nil, AcquireAndWaitToRelease);
    WaitForThreadTimeoutOrAcquisition;
    if vThread.LastResult = StatusAcquired then Result := vThread else begin
      vThread.FreeOnTerminate := True;
      vThread.ExecResume;
    end;
  end else if aSynchOBj is TCriticalSection then begin
    vThread := TEvsThreadTester.Create(False, aSynchOBj, cDefaultTimeOut, nil, CS_AcquireAndWaitToRelease);
    WaitForThreadTimeoutOrAcquisition;
    if vThread.LastResult = StatusAcquired then Result  := vThread else begin
      vThread.FreeOnTerminate := True;
      vThread.ExecResume;
    end;
  end;
end;

function ReadLock(const aObj:TObject):TLockHandle;
var
  vThread :TEvsThreadTester;
begin
  vThread := TEvsThreadTester.Create(False,aObj,cDefaultTimeOut,nil, AcquireRead);
  Result  := vThread;
  WaitForThreadTimeoutOrAcquisition;
end;

function WriteLock(const aobj:TObject):TLockHandle;
var
  vThread :TEvsThreadTester;
begin
  vThread := TEvsThreadTester.Create(False,aObj,cDefaultTimeOut,nil, AcquireWrite);
  Result  := vThread;
  WaitForThreadTimeoutOrAcquisition;
end;

function LockStatus(const aObj:Pointer):Integer;
begin
  InterlockedExchange(Result, StatusUnknown);
  if TObject(aObj) is TEvsThreadTester then  begin
    Result := TEvsThreadTester(aObj).LastResult;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure:   ReleaseLock;
  Author:      JKOZ
  DateTime:    2018.06.01
  Arguments:   aThread:Pointer
  Description: Takes a thread pointer returned by a call to LockXXXX,
               makes sure that it releases any lock it has on its
               synchronization object and self destructs. After this call,
               the pointer becomes invalid.
-------------------------------------------------------------------------------}
function ReleaseLock(var aThread:Pointer):Integer;
var
  vThrd : TEvsThreadTester;
begin
  Result := StatusUnknown;
  if TObject(aThread) is TEvsCounterThread then begin
    TEvsCounterThread(aThread).Action := ca_Quit;
  end;
  if TObject(aThread) is TEvsThreadTester then begin
    vThrd := TEvsThreadTester(aThread);
    InterlockedExchange(Integer(aThread), 0);
    vThrd.FreeOnTerminate := True;
    vThrd.ExecResume;
    Result := StatusReleased;
    WaitForThreadTimeoutOrAcquisition;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure:   IsMutexLocked;
  Author:      JKOZ
  DateTime:    2018.06.01
  Arguments:   aMutex:TMutex
  Description: is the object passed in aMutex locked? To answer this question we
               create a new thread, to avoid re entrant synch objects returning false
               information, and use it to get a lock, if the thread fails to get the lock
               then the object is locked otherwise its not.
-------------------------------------------------------------------------------}
Function IsMutexLocked(const aMutex:TMutex):Boolean;
var
  vThread:TEvsThreadTester;
  vMutex : TSemaphore;
begin
  Result := False;
  vMutex := TSemaphore.Create(0,1);
  try
    vThread := TEvsThreadTester.Create(False, aMutex, cDefaultTimeOut, vMutex, AcquireAndRelease);
    try
      sleep(10);    //wait for the thread to initialize properly.  The time out is system depended in slower cpus might be required
                    //to enlongate the wait to give a chance to the system scheduler to actually execute the thread.
      vMutex.Wait;  //wait until the thread finishes execution.
      Result := vThread.LastResult = StatusTimeOut;//if the thread was unable to acquire access then its locked by some one else.
    finally
      vThread.Free;
    end;
  finally
    vMutex.Free;
  end;
end;

{$REGION ' Critical Section Testing '}

function CG_LockSyncObject(const aObj:TSynchroObject):Pointer; // grabs the lock on a mutex and returns a pointer to the thread that has the lock.
var
  vThrd :TEvsThreadTester;
begin
  vThrd := TEvsThreadTester.Create(False, aObj, cDefaultTimeOut, nil, CS_AcquireAndWaitToRelease);
  Result := vThrd;
  Sleep(cDefaultTimeOut);//wait 10ms for the thread to reach the locked state and then exit; 
end;
function CG_IsLocked(const aMutex:TSynchroObject):Boolean;
var
  vThread:TEvsThreadTester;
  vMutex : TSemaphore;
begin
  Result := False;
  vMutex := TSemaphore.Create(0,1);
  try
    if aMutex is TCriticalSection then
      vThread := TEvsThreadTester.Create(False, aMutex, cDefaultTimeOut, vMutex, CS_AcquireAndRelease)
    else begin Result := False; Exit; end;
    try
      vMutex.Wait;
      Result := vThread.LastResult = StatusTimeOut;//if the thread was unable to acquire access then its locked by some one else.
    finally
      vThread.Free;
    end;
  finally
    vMutex.Free;
  end;
end;

{$ENDREGION}


{$REGION ' Thread Local Counter'}
//local thread counter
//creates a thread which increases the aCounter every aInterval milisecons by 1
//after that it goes to sleep until it receives a termination, dec or inc signal again.
function GetCounterLock(const aCounter:TObject; const aAmount, aInterval:DWORD):TLockHandle;
begin
  Result := TEvsCounterThread.Create(cSuspended, aCounter, aInterval, nil, CounterExec);
  TEvsCounterThread(Result).Action   := ca_Inc;
  TEvsCounterThread(Result).Count    := aAmount;
  TEvsCounterThread(Result).Resume;
end;

//given an existing counter thread it increases the counter by 1 for a aCounter times at aInterval miliseconds.
procedure IncreaseCounter(const aCounter:TLockHandle; const aAmount, aInterval:DWORD);
begin
  TEvsCounterThread(aCounter).Action   := ca_Inc;
  TEvsCounterThread(aCounter).FTimeOut := aInterval;
  TEvsCounterThread(aCounter).Count    := aAmount;
  TEvsCounterThread(aCounter).ExecResume;
end;

//given an existing counter thread it Decrease the counter by 1 for a aCounter times at aInterval miliseconds.
procedure DecreaseCounter(const aLock :TLockHandle; const aAmount, aDelay:DWORD);
begin
  TEvsCounterThread(aLock).Action   := ca_Dec;
  TEvsCounterThread(aLock).FTimeOut := aDelay;
  TEvsCounterThread(aLock).Count    := aAmount;
  TEvsCounterThread(aLock).ExecResume;
end;

procedure WaitFor(aMiliseconds:DWORD);
begin
  Sleep(aMiliseconds);
end;

function GetThreadID(const aLock :TLockHandle) :TThreadID;
begin
  if TObject(aLock) is TThread then Result := TThread(aLock).ThreadID;
  if aLock = nil               then Result := MainThreadID;
end;

{$ENDREGION}

{$ENDREGION}

end.
