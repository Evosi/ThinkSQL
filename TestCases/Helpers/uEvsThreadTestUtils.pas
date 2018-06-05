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
{
 a small units used to create thread to acquire and release locks from various synchronization objects for testing.
 all rights reserved.
}
unit uEvsThreadTestUtils;
{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ELSE}
  {$IFDEF WIN32} {$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  uEvsSyncObjs, SysUtils, Classes, Types,
  {$IFDEF Windows} Windows,{$ENDIF}
  SyncObjs;

const
  StatusUnknown   = -2;
  StatusFailed    = -1;
  StatusOK        =  0; //>0 acceptable states; <0 invalid or unexpected states; 0 = no state but everything looks ok.
  StatusAcquired  =  1;
  StatusReleased  =  2;
  StatusTimeOut   =  3;

  cDefaultTimeOut = 10;//ms we only need it for testing, if it takes longer then something is wrong with the code.

//Creates a thread and tries to get access to the mutex if it times out it returns true otherwise False.
function IsMutexLocked(const aMutex:TEvsMutex):Boolean;
function LockMutex(const aMutex:TEvsMutex):Pointer;   // grabs the lock on a mutex and returns a pointer to the thread that has the lock.
function LockObject(const aSynchOBj:TObject):Pointer; // grabs the lock on a synch object and returns a pointer to the thread that has the lock.
function ReleaseLock(var aThread:Pointer):Integer;    // the thread releases the mutex and self destructs, after that the pointer becomes invalid.
function LockStatus (const aObj:Pointer):Integer;

function ReadLock(const aObj:TObject):Pointer;
function WriteLock(const aobj:TObject):Pointer;

// functions for testing codegear synch objects
//   those should be merged in to one call
//   and the differences should become internal only.
function CG_LockSyncObject(const aObj:TSynchroObject):Pointer; // grabs the lock on a mutex and returns a pointer to the thread that has the lock.
function CG_IsLocked(const aMutex:TSynchroObject):Boolean;


implementation

type
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
    FLock     :TObject;       //lock to try and acquire.
    FResult   :Longint;       //if the acquire was succesfull this becomes true eitherwise false
    FTimeOut  :DWORD;         //how long will wait for the lock, usually 1000ms
    FMutex    :TEvsSemaphore; //when the execution finished this mutex will be released.
    FProc     :TThreadProc;   //which procedure to execute.
    FExecLock :TEvsSemaphore; //a semaphore that is locked to wait with the ability to get awaken from an other thread;
  protected
    procedure Execute;override;
    procedure ExecWait;
    procedure ExecResume;
  public
    constructor Create(CreateSuspended: Boolean; aLock:TObject; aTimeOut:DWORD; aMutex:TEvsSemaphore; aProc:TThreadProc);
    destructor Destroy; override;
    property LastResult :Longint read FResult;
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
  if aLock is TEvsSyncObj then begin
    if TEvsSyncObj(aLock).TryAcquire(aTimeOut) then begin
      InterlockedExchange(aResult, StatusAcquired);
      aThread.ExecWait;           // wait for confirmation, someone else will call the threads execresume.   //this code runs from inside the aThread if not something is very very wrong.
      TEvsSyncObj(aLock).Release; // undo any changes.
      InterlockedExchange(aResult, StatusReleased);
    end else aResult := StatusTimeOut;
  end else aResult := StatusFailed;
end;
{acquire and release at once}
procedure AcquireAndRelease(const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin //thread unsafe code do not use outside the testing environment.
  InterlockedExchange(aResult, StatusUnknown); // failed to try.
  if aLock is TEvsSyncObj then begin
    if TEvsSyncObj(aLock).TryAcquire(aTimeOut) then begin
      InterlockedExchange(aResult, StatusAcquired); //everything its ok.
      TEvsSyncObj(aLock).Release;
    end else InterlockedExchange(aResult, StatusTimeOut);
  end;// raise EEvsThreadTestException.Create('unknown lock object type');
end;
//procedure Acquire(const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
//begin //thread unsafe code do not use outside the testing environment.
//  InterlockedExchange(aResult, StatusUnknown); // failed to try.
//  if aLock is TEvsSyncObj then begin
//    if TEvsSyncObj(aLock).TryAcquire(aTimeOut) then begin
//      InterlockedExchange(aResult, StatusAcquired); //everything its ok.
//    end else InterlockedExchange(aResult, StatusTimeOut);
//  end;// raise EEvsThreadTestException.Create('unknown lock object type');
//end;
procedure Release(const aLock:Tobject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin //thread unsafe code do not use outside the testing environment.
  InterlockedExchange(aResult, StatusUnknown); // failed to try.
  try
    if aLock is TEvsSyncObj then begin
      TEvsSyncObj(aLock).Release;
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
  InterlockedExchange(aResult, StatusUnknown); // failed to try.
  try
    if aLock is TEvsMREWS then begin
      if TEvsMREWS(aLock).TryAcquireRead(cDefaultTimeOut) then
        InterlockedExchange(aResult, StatusAcquired) //everything its ok.
      else
        InterlockedExchange(aResult, StatusFailed);
      aThread.ExecWait; //pauses the thread execution and waits for a wake signal.
      if aThread.LastResult = StatusAcquired then TEvsMREWS(aLock).ReleaseRead;
    end
    ;// else InterlockedExchange(aResult, StatusFailed);
  except
    on E:Exception do InterlockedExchange(aResult, StatusFailed); // failed to try.
  end;
end;

procedure AcquireWrite(const aLock:TObject; aTimeOut:Integer; aThread:TEvsThreadTester; var aResult:Longint);
begin
  InterlockedExchange(aResult, StatusUnknown);
  try
    if aLock is TEvsMREWS then begin
      if TEvsMREWS(aLock).TryAcquireWrite(cDefaultTimeOut) then
        InterlockedExchange(aResult, StatusAcquired)
      else
        InterlockedExchange(aResult, StatusFailed);
      aThread.ExecWait;
      if aThread.LastResult = StatusAcquired then TEvsMREWS(aLock).ReleaseWrite;
    end else
      InterlockedExchange(aResult, StatusAcquired);
  except
    on E:Exception do InterlockedExchange(aResult, StatusFailed); // failed to try.
  end;
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

{$ENDREGION}

{$REGION ' TEvsThreadTester '}


{-------------------------------------------------------------------------------
  Procedure: TEvsThreadTester.Create
  Author:    JKOZ
  DateTime:  2018.06.01
  Arguments: CreateSuspended: Boolean;        Create the thread suspended or not.
             aLock:TEvsSyncObj;               Object to try and lock
             aTimeOut:DWORD;                  How long it will wait for the lock
             aMutex:TEvsSemaphore             This will be released upon complition of execution. after that the thread can be destroyed.
-------------------------------------------------------------------------------}
constructor TEvsThreadTester.Create(CreateSuspended: Boolean; aLock:TObject; aTimeOut:DWORD; aMutex:TEvsSemaphore; aProc:TThreadProc);
begin
  inherited Create(CreateSuspended);
  Flock     := aLock;
  FTimeOut  := aTimeOut;
  FMutex    := aMutex;
  FProc     := aProc;
  FExecLock := TEvsSemaphore.Create(0,1); //it comes already locked then next thread should wait.
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
  Procedure:   TEvsThreadTester.Execute;
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

{-------------------------------------------------------------------------------
  Procedure:   TEvsThreadTester.Execute;
  Author:      JKOZ
  DateTime:    2018.06.01
  Arguments:   None
  Description: is the passed sync object locked? This assumes that the
               thread already tried to wait for on the sync object and either
               acquired the lock or it failed to acquire it.
-------------------------------------------------------------------------------}
//function TEvsThreadTester.GetLocked: Boolean;deprecated;
//begin
//  Result := FResult in [StatusOK, StatusAcquired];
//end;

// all the interface declared functions will be in the global region.
{$ENDREGION}

{$REGION ' Global section '}
Function LockMutex(const aMutex:TEvsMutex):Pointer;
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
  if aSynchObj is TEvsSyncObj then begin
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

function ReadLock(const aObj:TObject):Pointer;
var
  vThread :TEvsThreadTester;
begin
  vThread := TEvsThreadTester.Create(False,aObj,cDefaultTimeOut,nil, AcquireRead);
  Result  := vThread;
  WaitForThreadTimeoutOrAcquisition;
end;

function WriteLock(const aobj:TObject):Pointer;
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
  Arguments:   aMutex:TEvsMutex
  Description: is the object passed in aMutex locked? To answer this question we
               create a new thread, to avoid re entrant synch objects returning false
               information, and use it to get a lock, if the thread fails to get the lock
               then the object is locked otherwise its not.
-------------------------------------------------------------------------------}
Function IsMutexLocked(const aMutex:TEvsMutex):Boolean;
var
  vThread:TEvsThreadTester;
  vMutex : TEvsSemaphore;
begin
  Result := False;
  vMutex := TEvsSemaphore.Create(0,1);
  try
    vThread := TEvsThreadTester.Create(False, aMutex, cDefaultTimeOut, vMutex, AcquireAndRelease);
    try
      sleep(10);
      vMutex.Wait;
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
  vMutex : TEvsSemaphore;
begin
  Result := False;
  vMutex := TEvsSemaphore.Create(0,1);
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

{$ENDREGION}

end.
