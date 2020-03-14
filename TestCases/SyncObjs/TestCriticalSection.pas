unit TestCriticalSection;
{ Evosi MultiThread primitives Test Case
  ----------------------
  This unit contains The test case for delphi's TCriticalSection this is a very basic
  test case that makes sure that
    1) once a thread has entered in the critical section no other thread can,
    2) the same thread can re enter with out problems. }

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF}
  TestFramework, SysUtils, SyncObjs, Classes;

type
  // Test methods for class TCriticalSection
  
  TestTCriticalSection = class(TTestCase)
  strict private
    FCriticalSection: TCriticalSection;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcquire;
    procedure TestRelease;
    procedure TestTryEnter;
    procedure TestEnter;
    procedure TestLeave;
    procedure TestReEnter;
  end;

implementation
uses uEvsThreadTestUtils;

procedure TestTCriticalSection.SetUp;
begin
  FCriticalSection := TCriticalSection.Create;
end;

procedure TestTCriticalSection.TearDown;
begin
  FCriticalSection.Free;
  FCriticalSection := nil;
end;

procedure TestTCriticalSection.TestAcquire;
begin
  FCriticalSection.Acquire;
  CheckTrue(CG_IsLocked(FCriticalSection),'Other threads can take Control?');//tries to get a lock from a secondary thread.
  FCriticalSection.Release;
  // TODO: Validate method results
end;

procedure TestTCriticalSection.TestReEnter;
begin
  FCriticalSection.Acquire;
  CheckTrue(CG_IsLocked(FCriticalSection),'Seems that you dig not lock it properly mate.');
  CheckTrue(FCriticalSection.TryEnter,'Did you locked it from an other thread?');//re enter a couple of times
  CheckTrue(FCriticalSection.TryEnter,'Did you locked it from an other thread?');
  CheckTrue(FCriticalSection.TryEnter,'Did you locked it from an other thread?');
  CheckTrue(CG_IsLocked(FCriticalSection),'Seems that you dig not lock it properly mate.');
  FCriticalSection.Release;
  CheckTrue(CG_IsLocked(FCriticalSection),'Seems that you dig not lock it properly mate.');
  FCriticalSection.Release;
  CheckTrue(CG_IsLocked(FCriticalSection),'Seems that you dig not lock it properly mate.');
  FCriticalSection.Release;
  CheckTrue(CG_IsLocked(FCriticalSection),'Seems that you dig not lock it properly mate.');
  FCriticalSection.Release;
  CheckFalse(CG_IsLocked(FCriticalSection),'Seems that you dig not lock it properly mate.');
 end;

procedure TestTCriticalSection.TestRelease;
begin
  FCriticalSection.Acquire;
  CheckTrue(CG_IsLocked(FCriticalSection),'Other threads can take Control?');
  FCriticalSection.Release;
  CheckFalse(CG_IsLocked(FCriticalSection),'Other threads can''t take Control?');
end;

procedure TestTCriticalSection.TestTryEnter;
var
  vObj : Pointer;
begin
  vObj := CG_LockSyncObject(FCriticalSection);
  try
    CheckFalse(FCriticalSection.TryEnter,'Did not lock?');
  finally
    ReleaseLock(vObj);
    Check(vObj = nil,'You need to change the ReleaseLock and clean up.');
  end;
  CheckTrue(FCriticalSection.TryEnter,'Did not Release?');
  CheckTrue(CG_IsLocked(FCriticalSection),'Did not lock?');
  FCriticalSection.Leave;
end;

procedure TestTCriticalSection.TestEnter;
begin
  FCriticalSection.Enter;
  CheckTrue(CG_IsLocked(FCriticalSection),'Other threads can take Control?');
  FCriticalSection.Leave;
  // TODO: Validate method results
end;

procedure TestTCriticalSection.TestLeave;
begin
  FCriticalSection.Enter;
  CheckTrue(CG_IsLocked(FCriticalSection),'Other threads can take Control?');
  FCriticalSection.Leave;
  CheckFalse(CG_IsLocked(FCriticalSection),'Other threads can''t take Control?');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCriticalSection.Suite);
end.

