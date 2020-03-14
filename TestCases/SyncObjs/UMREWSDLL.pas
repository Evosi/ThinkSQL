unit UMREWSDLL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { TDelphiMREWS }

  TDelphiMREWS = Class
  private
    FLock :Pointer;
  public
    constructor Create;
    function StartRead  :Boolean;
    function StartWrite :Boolean;
    function EndRead    :Boolean;
    function EndWrite   :Boolean;
  end;

implementation
type
  TLockHandle = Pointer;

function Mrews_Create (var aLock:TLockHandle):Integer;           stdcall; external 'MREWS.dll';
function Mrews_Destroy(var aLock:TLockHandle):Integer;           stdcall; external 'MREWS.dll';
function Mrews_StartRead(const aLock:TLockHandle):Integer;       stdcall; external 'MREWS.dll';
function Mrews_StartWrite(const aLock:TLockHandle):Integer;      stdcall; external 'MREWS.dll';
function Mrews_FinishRead(const aLock:TLockHandle):Integer;      stdcall; external 'MREWS.dll';
function Mrews_FinishWrite(const aLock:TLockHandle):Integer;     stdcall; external 'MREWS.dll';

{ TDelphiMREWS }

constructor TDelphiMREWS.Create;
var
  vRes:Integer;
begin
  vRes := Mrews_Create(Flock);
  if vRes <> 0 then raise Exception.CreateFmt('Unknown error : %D',[vRes]);
  inherited;
end;

function TDelphiMREWS.StartRead :Boolean;
var
  vRes:Integer;
begin
  vRes := Mrews_StartRead(FLock);
  Result := vRes = 0;
end;

function TDelphiMREWS.StartWrite :Boolean;
var
  vRes:Integer;
begin
  vRes := Mrews_StartWrite(FLock);
  Result := vRes = 0;
end;

function TDelphiMREWS.EndRead :Boolean;
var
  vRes:Integer;
begin
  vRes := Mrews_FinishRead(FLock);
  Result := vRes = 0;
end;

function TDelphiMREWS.EndWrite :Boolean;
var
  vRes:Integer;
begin
  vRes := Mrews_FinishWrite(FLock);
  Result := vRes = 0;
end;


end.

