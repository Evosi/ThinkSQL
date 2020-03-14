unit UTestDelphiMREWS;
{ This unit contains the test cases for the multi read exclusive write synhronizer,
  as alwasy this is work in progress, any problems you can demostrate or features you wish to see
  are welcome, above all else I welcome test cases that will help me debug your problems. }

interface

uses
  TestFramework,
  sysutils,
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  UMREWSDLL, SyncObjects, uThreadTestUtils;

type
  // Test methods for class TEvsSemaSynchronizer

  { TTestEvsMREWSynchronizer }

  TTestEvsMREWSynchronizer = class(TTestCase)
  strict private
    FSynchronizer: TDelphiMREWS;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReadLock;              //test multiple readers and the counter is properly updated.
    procedure TestWriteLock;             //test single writer with re entry and the counter is properly updated.
    procedure TestSingleThreadReEntry;   //test that a single thread can acquire read and write access but other threads do not have neither.
    procedure TestSingleThreadWriteLock; // no other threads can read or write.
  end;

implementation

procedure TTestEvsMREWSynchronizer.SetUp;
begin
  FSynchronizer := TDelphiMREWS.Create;
end;

procedure TTestEvsMREWSynchronizer.TearDown;
begin
  FSynchronizer.Free;
  FSynchronizer := nil;
end;

procedure TTestEvsMREWSynchronizer.TestReadLock;
var
  vLock:Array[0..9] of TLockHandle;
begin
   // be carefull which thread gets the lock first
  vLock[0] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[0]),'This is multiple read and failed on the 2nd?');
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'already locked?');
  vLock[1] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[1]),'This is multiple read and failed on the 2nd?');
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'Failed of the 3rd?');
  vLock[2] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[2]),'This is multiple read and failed on the 4Th?');
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'Failed of the 5th?');
  vLock[3] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[3]),'This is multiple read and failed on the 6th?');
  CheckTrue(FSynchronizer.TryAcquireRead(10), 'Failed of the 7th?');
  vLock[4] := ReadLock(FSynchronizer);
  CheckEquals(StatusAcquired, LockStatus(vlock[4]),'This is multiple read and failed on the 8th?');
  //reentrant on read? check the read count.
  CheckEquals(9,FSynchronizer.ReadCounter,'erm 8 succesfull read locks and only '+IntToStr(FSynchronizer.ReadCounter)+' counted?');
  //CheckFalse(FSynchronizer.TryAcquireWrite(10),'what?? you can acquire a write lock with all those threads inside?');
//-------------------------------------
  ReleaseLock(vLock[0]);      //1
  CheckEquals(8, FSynchronizer.ReadCounter, '1st lock did not release?');
  FSynchronizer.ReleaseRead;  //2
  CheckEquals(7, FSynchronizer.ReadCounter, '2nd lock did not release?');
  FSynchronizer.ReleaseRead;  //3
  CheckEquals(6, FSynchronizer.ReadCounter, '3rd lock did not release?');
  ReleaseLock(vLock[1]);      //4
  CheckEquals(5, FSynchronizer.ReadCounter, '4th lock did not release?');
  FSynchronizer.ReleaseRead;  //5
  CheckEquals(4, FSynchronizer.ReadCounter, '5th lock did not release?');
  ReleaseLock(vLock[2]);      //6
  CheckEquals(3, FSynchronizer.ReadCounter, '6th lock did not release?');
  FSynchronizer.ReleaseRead;  //7
  CheckEquals(2, FSynchronizer.ReadCounter, '7th lock did not release?');
  ReleaseLock(vLock[3]);      //8
  CheckEquals(1, FSynchronizer.ReadCounter, '8th lock did not release?');
  ReleaseLock(vLock[4]);      //9
  CheckEquals(0, FSynchronizer.ReadCounter, '9th lock did not release?');
end;

procedure TTestEvsMREWSynchronizer.TestWriteLock;
var
  vLock: TLockHandle;
begin
  CheckTrue(FSynchronizer.TryAcquireWrite(10), 'First write lock and it failed?');          //lock it from the main thread
  vLock := WriteLock(FSynchronizer);                                                        //try to lock it from a secondary thread
  CheckEquals(StatusFailed, LockStatus(vLock), 'Well, this suppose to be single writer.');  //check that the secondary thread lock failed.
  CheckTrue(FSynchronizer.TryAcquireWrite(10), 'What happend to re entry?');                //check re entrant write lock from the same thread.
  ReleaseLock(vLock);                                                                       //free the failed secondary thread.
  FSynchronizer.ReleaseWrite;                                                               //Free the second lock of the main thread.
  vLock := WriteLock(FSynchronizer);                                                        //try to lock from a secondary thread.
  CheckEquals(StatusFailed, LockStatus(vLock), 'You forgot how to count or something?.');   //check that the main thread has not freed the lock yet.
  ReleaseLock(vLock);                                                                       //release the secondary thread.
  FSynchronizer.ReleaseWrite;                                                               //Release the main thread's first lock.
  vLock := WriteLock(FSynchronizer);                                                        //acquire the lock from a secondary thread.
  CheckEquals(StatusAcquired, LockStatus(vLock),'What happened to the release above?');     //check that the secondary thread locked it successfully
  CheckFalse(FSynchronizer.TryAcquireWrite(10),'erm is your thread lock active?');          //make sure that the main thread can not acquire a lock any longer.
  ReleaseLock(vLock);                                                                       //release the secondary thread's lock and destroy the thread.
  CheckTrue(FSynchronizer.TryAcquireWrite(10),'erm is thread utils working properly?');     //Check that our thread utils correctly released the lock with the secondary thread.
  vLock := ReadLock(FSynchronizer);                                                         //try to acquire a read lock from a secondary thread.
  CheckEquals(StatusFailed, LockStatus(vLock),'read Lock acquired wiuth write lock?');      //check that the secondary thread read lock failed.
  ReleaseLock(vLock);                                                                       //destroy the secondary thread.
  FSynchronizer.ReleaseWrite;                                                               //Finally release the last lock and exit.
end;

procedure TTestEvsMREWSynchronizer.TestSingleThreadReEntry;
var
  vLock:Array[0..9] of TLockHandle;
begin
  CheckEquals(0, FSynchronizer.ReadCounter,'Unmet test conditions. Synchronizer has active readers.');
  vLock[0] := WriteLock(FSynchronizer);

  CheckEquals(0, FSynchronizer.ReadCounter,'Test process failed to release all readers.');
end;

procedure TTestEvsMREWSynchronizer.TestSingleThreadWriteLock;
var
  vTst:syncobj.tmultiread
begin

end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestEvsMREWSynchronizer.Suite);
end.

