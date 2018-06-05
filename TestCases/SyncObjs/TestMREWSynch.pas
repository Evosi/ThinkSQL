
unit TestMREWSynch;
{ This unit contains the test cases for the multi read aexclusive write  synhronizer,
  as alwasy this is work in progress, any problems you can demostrate or features you wish to see
  are welcome, above all else I welcome test cases that will help me debug your problems. }

interface

uses
  TestFramework,
  sysutils,
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  uEvsSyncObjs, uEvsThreadTestUtils;

type
  // Test methods for class TEvsSemaSynchronizer
  
  TTestEvsMREWSynchronizer = class(TTestCase)
  strict private
    FSynchronizer: TEvsMREWS;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStartRead;
    procedure TestStartWrite;
    procedure TestEndRead;
    procedure TestEndWrite;
  end;

implementation

procedure TTestEvsMREWSynchronizer.SetUp;
begin
  FSynchronizer := TEvsMREWS.Create;
end;

procedure TTestEvsMREWSynchronizer.TearDown;
begin
  FSynchronizer.Free;
  FSynchronizer := nil;
end;

procedure TTestEvsMREWSynchronizer.TestStartRead;
var
  vLock:Array[0..10] of Pointer;
begin
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'already locked?');
  vLock[0] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[0]),'This is multiple read and failed on the 2nd?');
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'Failed of the 3rd?');
  vLock[1] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[1]),'This is multiple read and failed on the 4Th?');
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'Failed of the 5th?');
  vLock[2] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[2]),'This is multiple read and failed on the 6th?');
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'Failed of the 7th?');
  vLock[3] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[3]),'This is multiple read and failed on the 8th?');
  //reentrant on read? check the read count.
  CheckEquals(8,FSynchronizer.ReadCounter,'erm 8 succesfull read locks and only '+IntToStr(FSynchronizer.ReadCounter)+' counted?');
//-------------------------------------
  FSynchronizer.ReleaseRead;  //1
  CheckEquals(7, FSynchronizer.ReadCounter, '1st lock did not release?');
  ReleaseLock(vLock[0]);      //2
  CheckEquals(6, FSynchronizer.ReadCounter, '2nd lock did not release?');
  FSynchronizer.ReleaseRead;  //3
  CheckEquals(5, FSynchronizer.ReadCounter, '3rd lock did not release?');
  ReleaseLock(vLock[1]);      //4
  CheckEquals(4, FSynchronizer.ReadCounter, '4th lock did not release?');
  FSynchronizer.ReleaseRead;  //5
  CheckEquals(3, FSynchronizer.ReadCounter, '5th lock did not release?');
  ReleaseLock(vLock[2]);      //6
  CheckEquals(2, FSynchronizer.ReadCounter, '6th lock did not release?');
  FSynchronizer.ReleaseRead;  //7
  CheckEquals(1, FSynchronizer.ReadCounter, '7th lock did not release?');
  ReleaseLock(vLock[3]);      //8
  CheckEquals(0, FSynchronizer.ReadCounter, '8th lock did not release?');
end;

procedure TTestEvsMREWSynchronizer.TestStartWrite;
var
  vLock: Pointer;
begin
  Fail('You need some write acquisition tests');
  CheckTrue(FSynchronizer.TryAcquireWrite(10), 'First write lock and it failed?');
  vLock := WriteLock(FSynchronizer);
  CheckEquals(StatusFailed, LockStatus(vLock), 'Well, this suppose to be single writer.');
  ReleaseLock(vLock);
  FSynchronizer.ReleaseWrite;
end;

procedure TTestEvsMREWSynchronizer.TestEndRead;
var
  vLock:Array[0..10] of Pointer;
begin
  fail('Make sure that the release of read locks is in order');
  FSynchronizer.AcquireWrite;
  vLock[0] := WriteLock(FSynchronizer);
  FSynchronizer.AcquireWrite;
  vLock[1] := WriteLock(FSynchronizer);
  FSynchronizer.AcquireWrite;
  vLock[2] := WriteLock(FSynchronizer);
  FSynchronizer.AcquireWrite;
  vLock[3] := WriteLock(FSynchronizer);
//-------------------------------------
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[0]);
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[1]);
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[2]);
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[3]);
end;

procedure TTestEvsMREWSynchronizer.TestEndWrite;
var
  vLock:Array[0..10] of Pointer;
begin
  Fail('Make sure the Release of write locks is in order');
  FSynchronizer.AcquireWrite;
  vLock[0] := WriteLock(FSynchronizer);
  FSynchronizer.AcquireWrite;
  vLock[1] := WriteLock(FSynchronizer);
  FSynchronizer.AcquireWrite;
  vLock[2] := WriteLock(FSynchronizer);
  FSynchronizer.AcquireWrite;
  vLock[3] := WriteLock(FSynchronizer);
//-------------------------------------
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[0]);
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[1]);
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[2]);
  FSynchronizer.ReleaseWrite;
  ReleaseLock(vLock[3]);
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TTestEvsMREWSynchronizer.Suite);
end.

