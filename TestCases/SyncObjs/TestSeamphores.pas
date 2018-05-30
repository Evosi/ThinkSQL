unit TestSeamphores;

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ELSE}
{$IFDEF Win32}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}
interface

uses
  Classes, SysUtils, TestFramework, uEvsSyncObjs;

type

  TTestEvsSemaphore = class(TTestCase)
  strict private
    FEvsSemaphore: TEvsSemaphore;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAquire;
    procedure TestWait;
    procedure TestRelease;
    procedure TestSignal;
  end;

implementation
{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}//function SemaGetValue(aSem:TSemaphoreHandle):Integer;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
//begin
//  OSCheck(Sem_GetValue(aSem, Result)=0);
//end;

//  SemGetValue  : TSemGetValue;      //return the semaphore value this should not be here.
{$IFDEF WINDOWS}
type
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

function SemGetValue(aSem:TSemaphoreHandle):Integer;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
  vSemInfo : TSemaphoreBasicInformation;
  ntStatus : Integer;
  vLen     : Integer;
begin
  Result := -1;
  ntStatus := NtQuerySemaphore(aSem, SemaphoreBasicInformation, @vSemInfo, SizeOf(vSemInfo), @vLen);
  if ntStatus = 0 then Result := vSemInfo.CurrentCount else RaiseLastOSError;
end;
{$ENDIF}

procedure TTestEvsSemaphore.SetUp;
begin
  FEvsSemaphore := TEvsSemaphore.Create(5,5);
end;

procedure TTestEvsSemaphore.TearDown;
begin
  FEvsSemaphore.Free;
  FEvsSemaphore := nil;
end;

procedure TTestEvsSemaphore.TestAquire;
begin
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(4,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(3,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(2,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(1,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(0,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  CheckEquals(False, FEvsSemaphore.TryAcquire(100),IntToStr(SemGetValue(FEvsSemaphore.Handle)));
  // TODO: Validate method results
end;

procedure TTestEvsSemaphore.TestWait;
begin
  CheckTrue(FEvsSemaphore.TryAcquire(1000));

  FEvsSemaphore.Acquire;
  // TODO: Validate method results
end;

procedure TTestEvsSemaphore.TestRelease;
begin
  CheckEquals(5,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(4,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(3,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(2,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(1,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(0,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckEquals(False, FEvsSemaphore.TryAcquire(100),IntToStr(SemGetValue(FEvsSemaphore.Handle))); //fail no more resources.
  FEvsSemaphore.Release;                                              //release
  CheckEquals(1,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Release;                                              //release
  CheckEquals(2,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Release;                                              //release
  CheckEquals(3,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Release;                                              //release
  CheckEquals(4,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Release;                                              //release
  CheckEquals(5,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  StartExpectingException(EOSError);                                  //next release must raise and error the counter riched its limit.
  FEvsSemaphore.Release;
end;

procedure TTestEvsSemaphore.TestSignal;
begin
  CheckEquals(5,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(4,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(3,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(2,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(1,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckTrue(FEvsSemaphore.TryAcquire(10));
  CheckEquals(0,SemGetValue(FEvsSemaphore.Handle), 'invalid count');  //acquire
  CheckEquals(False, FEvsSemaphore.TryAcquire(100),IntToStr(SemGetValue(FEvsSemaphore.Handle))); //fail no more resources.
  FEvsSemaphore.Signal;                                              //release
  CheckEquals(1,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Signal;                                              //release
  CheckEquals(2,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Signal;                                              //release
  CheckEquals(3,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Signal;                                              //release
  CheckEquals(4,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  FEvsSemaphore.Signal;                                              //release
  CheckEquals(5,SemGetValue(FEvsSemaphore.Handle), 'invalid count');
  StartExpectingException(EOSError);                                 //next release must raise and error the counter riched its limit.
  FEvsSemaphore.Signal;
end;

initialization
  RegisterTest(TTestEvsSemaphore.Suite);
end.

