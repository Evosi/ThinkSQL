unit uEvsThreadTestUtils;

interface

uses uEvsSyncObjs, SysUtils, Classes, Types;

Function IsMutexLocked(const aMutex:TEvsMutex):Boolean;

implementation

type
  TEvsThreadTester = class(TThread)
  private
    FLock    :TEvsSyncObj;
    FLocked  :Boolean;
    FTimeOut :DWORD;
    FMutex   :TEvsSemaphore;
  protected
    procedure Execute;override;
  public
    constructor Create(CreateSuspended: Boolean; aLock:TEvsSyncObj; aTimeOut:DWORD; aMutex:TEvsSemaphore);
    property Locked : Boolean read FLocked;
  end;

Function IsMutexLocked(const aMutex:TEvsMutex):Boolean;
var
  vThread:TEvsThreadTester;
  vMutex : TEvsSemaphore;
begin
  Result := False;
  vMutex := TEvsSemaphore.Create(0,1);
  vThread := TEvsThreadTester.Create(False, aMutex, 1000, vMutex);
  vMutex.Wait;
  Result := vThread.FLocked;
end;

{ TEvsThreadTester }

constructor TEvsThreadTester.Create(CreateSuspended: Boolean; aLock:TEvsSyncObj; aTimeOut:DWORD; aMutex:TEvsSemaphore);
begin
  inherited Create(CreateSuspended);
  Flock := aLock;
  FTimeOut := aTimeOut;
  FMutex := aMutex;
end;

procedure TEvsThreadTester.Execute;
begin
  inherited;
  FLocked := not FLock.TryAcquire(FTimeOut);
  FMutex.Release;//or signal we are done
end;

end.
