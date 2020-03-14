{ ╔════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗
  ║                                        Copyright© 2018 EVOSI® all rights reserved                                  ║
  ╠════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║                                                                                                                    ║
  ║                   ▄▄▄▄▄▄▄▄▄▄▄    ▄               ▄    ▄▄▄▄▄▄▄▄▄▄▄     ▄▄▄▄▄▄▄▄▄▄▄    ▄▄▄▄▄▄▄▄▄▄▄                   ║
  ║                  ▐░░░░░░░░░░░▌  ▐░▌             ▐░▌  ▐░░░░░░░░░░░▌   ▐░░░░░░░░░░░▌  ▐░░░░░░░░░░░▌                  ║
  ║                  ▐░█▀▀▀▀▀▀▀▀▀    ▐░▌           ▐░▌   ▐░█▀▀▀▀▀▀▀█░▌   ▐░█▀▀▀▀▀▀▀▀▀    ▀▀▀▀█░█▀▀▀▀                   ║
  ║                  ▐░▌              ▐░▌         ▐░▌    ▐░▌       ▐░▌   ▐░▌                 ▐░▌                       ║
  ║                  ▐░█▄▄▄▄▄▄▄▄▄      ▐░▌       ▐░▌     ▐░▌       ▐░▌   ▐░█▄▄▄▄▄▄▄▄▄        ▐░▌                       ║
  ║                  ▐░░░░░░░░░░░▌      ▐░▌     ▐░▌      ▐░▌       ▐░▌   ▐░░░░░░░░░░░▌       ▐░▌                       ║
  ║                  ▐░█▀▀▀▀▀▀▀▀▀        ▐░▌   ▐░▌       ▐░▌       ▐░▌    ▀▀▀▀▀▀▀▀▀█░▌       ▐░▌                       ║
  ║                  ▐░▌                  ▐░▌ ▐░▌        ▐░▌       ▐░▌             ▐░▌       ▐░▌                       ║
  ║                  ▐░█▄▄▄▄▄▄▄▄▄          ▐░▐░▌         ▐░█▄▄▄▄▄▄▄█░▌    ▄▄▄▄▄▄▄▄▄█░▌   ▄▄▄▄█░█▄▄▄▄                   ║
  ║                  ▐░░░░░░░░░░░▌          ▐░▌          ▐░░░░░░░░░░░▌   ▐░░░░░░░░░░░▌  ▐░░░░░░░░░░░▌                  ║
  ║                   ▀▀▀▀▀▀▀▀▀▀▀            ▀            ▀▀▀▀▀▀▀▀▀▀▀     ▀▀▀▀▀▀▀▀▀▀▀    ▀▀▀▀▀▀▀▀▀▀▀                   ║
  ║                                                                                                                    ║
  ╚════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝

 Test case for the thread local counters class.
 This class keeps double accounting, one for the total amount
 and one for the amount each thread contributed to the total.
}
unit TestThreadLocalCount;

{$I Defs.inc}

interface

uses
  Classes, SysUtils, TestFramework, uSyncObjs, uThreadTestUtils;

type
  { TTThreadLocalCounters }
  TTThreadLocalCounters = class(TTestCase)
  private
    FLocalCounter:TThreadLocalCounters;
  protected
    procedure SetUp;    override;
    procedure TearDown; override;
  published
    procedure CheckThreadTotal;
    procedure TestTotal2;
    procedure TestTotal;
  end;

implementation

procedure TTThreadLocalCounters.CheckThreadTotal;
var
  vCntr  :Integer;
begin
  for vCntr := 0 to 9 do
    FLocalCounter.Increase;
  CheckEquals(10, FLocalCounter.Total, 'Total');
  CheckEquals(10, FLocalCounter.ThreadTotal, 'ThreadTotal');
end;

procedure TTThreadLocalCounters.SetUp;
begin
  inherited SetUp;
  FLocalCounter := TThreadLocalCounters.Create;
end;

procedure TTThreadLocalCounters.TearDown;
begin
  FLocalCounter.Free;
  inherited TearDown;
end;

procedure TTThreadLocalCounters.TestTotal;
const
   cDefaultWait  = 50;
   cDefaultCount = 10;
   cWait         = (cDefaultCount * cDefaultWait) + (cDefaultWait + 10 );
var
  vCntr  :Integer;
  vLocks :array of TLockHandle;
begin
  SetLength(vLocks,3);

  vLocks[0] := GetCounterLock(FLocalCounter, cDefaultCount, cDefaultWait);
  WaitFor(cWait + 10) ;
  CheckEquals(cDefaultCount, FLocalCounter.Total,       '1st Total');
  CheckEquals(0,             FLocalCounter.ThreadTotal, '1st ThreadTotal');

  vLocks[1] := GetCounterLock(FLocalCounter,cDefaultCount,cDefaultWait);
  WaitFor(cWait);
  CheckEquals(2*cDefaultCount, FLocalCounter.Total,       '2nd Total');
  //CheckEquals(0,               FLocalCounter.ThreadTotal, '2nd ThreadTotal');

  vLocks[2] := GetCounterLock(FLocalCounter,10,cDefaultWait);
  WaitFor(cWait);
  CheckEquals(3*cDefaultCount, FLocalCounter.Total,       '3rd Total');
  CheckEquals(0,               FLocalCounter.ThreadTotal, '3rd ThreadTotal');  //sanity check

  for vCntr := 0 to cDefaultCount -1 do
    FLocalCounter.Increase;
  CheckEquals(4*cDefaultCount, FLocalCounter.Total, '4th Total');
  CheckEquals(cDefaultCount,   FLocalCounter.ThreadTotal, '4th ThreadTotal');

  DecreaseCounter(vLocks[0], cDefaultCount, cDefaultWait);
  WaitFor(cWait);
  CheckEquals(3*cDefaultCount, FLocalCounter.Total,       '5th Total');
  //CheckEquals(cDefaultCount,   FLocalCounter.ThreadTotal, '5th ThreadTotal');

  DecreaseCounter(vLocks[1], cDefaultCount, cDefaultWait);
  WaitFor(cWait);
  CheckEquals(2*cDefaultCount, FLocalCounter.Total,       '6th Total');
  //CheckEquals(cDefaultCount,   FLocalCounter.ThreadTotal, '6th ThreadTotal');

  DecreaseCounter(vLocks[2], cDefaultCount, cDefaultWait);
  WaitFor(cWait);
  CheckEquals(cDefaultCount, FLocalCounter.Total,       '7th Total');
  CheckEquals(cDefaultCount, FLocalCounter.ThreadTotal, '7th ThreadTotal');//sanity check


  for vCntr := 0 to 9 do
    FLocalCounter.Decrease;
  CheckEquals(0, FLocalCounter.Total,       '8th Total');
  CheckEquals(0, FLocalCounter.ThreadTotal, '8th ThreadTotal');


  ReleaseLock(vLocks[0]);
  ReleaseLock(vLocks[1]);
  ReleaseLock(vLocks[2]);
  WaitFor(cWait);
end;

procedure TTThreadLocalCounters.TestTotal2;
var
  vCntr  :Integer;
  vLocks :array of TLockHandle;

  procedure SpinWait(const aValue:Integer);
  begin
    while FLocalCounter.Total <> aValue do begin
      Sleep(1);
    end;
  end;

begin
  SetLength(vLocks, 10);
  for vCntr := 0 to 9 do begin
    vLocks[vCntr] := GetCounterLock(FLocalCounter,10,50);
    FLocalCounter.Increase;
  end;

  SpinWait(110);
  CheckEquals(110,FLocalCounter.Total,'1 Total');
  CheckEquals(10,FLocalCounter.ThreadTotal,'1 ThreadTotal');


  for vCntr := 0 to 9 do begin
    DecreaseCounter(vLocks[vCntr], 10, 50);// := GetCounterLock(FLocalCounter,10,50);
  end;
  SpinWait(10);
  CheckEquals(10,FLocalCounter.Total,'2 Total');
  CheckEquals(10,FLocalCounter.ThreadTotal,'2 ThreadTotal');

  for vCntr := 0 to 9 do begin
    FLocalCounter.Decrease
  end;
  CheckEquals(0, FLocalCounter.Total,'3 Total');
  CheckEquals(0, FLocalCounter.ThreadTotal,'3 ThreadTotal');
end;

initialization
  RegisterTest(TTThreadLocalCounters.Suite);

end.

