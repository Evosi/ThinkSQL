unit uEvsThsOptions;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface
  {EVS is the prefix for all the units/class I add to the project
   Ths is the prefix for ThinkSQL, it is used mostly for classes that
   are thinksql specific and not generic
  }
uses
  Classes, SysUtils, IdGlobal, uGlobal;

type
  IEvsOptionPersister = interface;

  IEvsPersistable = Interface
  ['{4BDCA43A-4F49-46AB-9064-239A784C3C47}']
    procedure SaveTo(const aPersister: IEvsOptionPersister);    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure LoadFrom(const aPersister:IEvsOptionPersister);  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  end;

  { IEvsThinkSQLOptionPersister }
  //Generic interface to save options. concrete implementation will support ini, json and some sort of database back ends.
  IEvsOptionPersister = interface(IUnknown)
  ['{0012F0B0-2791-4542-8F8D-5DEBAA3CC1A7}']
    procedure WriteString  (const aKey, aValue:WideString);                                 {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteInteger (const aKey:WideString; const aValue:Integer);                   {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteDouble  (const aKey:WideString; const aValue:Double);                    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteDateTime(const aKey:WideString; const aValue:TDateTime);                 {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteInt64   (const aKey:WideString; const aValue: Int64);                    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteBoolean (const aKey:WideString; const aValue: Boolean);                  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    function ReadString   (const aKey, aDefaultValue:WideString):wideString;                {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadInteger  (const aKey:WideString;const aDefaultValue:Integer)  :Integer;    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadDouble   (const aKey:WideString;const aDefaultValue:Double)   :Double;     {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadDateTime (const aKey:WideString;const aDefaultValue:TDateTime):TDateTime;  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadInt64    (const aKey:WideString;const aDefaultValue:Int64)    : Int64;     {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadBoolean  (const aKey:WideString;const aDefaultValue:Boolean)  : Boolean;   {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    procedure Write (const aKey:WideString; const aBuffer; const aSize:integer);          {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function  Read  (const aKey:WideString; var aBuffer; const aBufSize:integer):Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    procedure Load(const aSubject:IEvsPersistable); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure Save(const aSubject:IEvsPersistable); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  end;

  { IEvsThsRuntimeOptions }

  IEvsThsRuntimeOptions = interface(IEvsPersistable)
  ['{681F14F3-DF81-46AB-8510-A022ACCB35CA}']
    function GetAutoCreate :Boolean;                 {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetDataDirectory :widestring;           {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetDefaultDBName :widestring;           {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetDefaultExt :Widestring;              {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetPort :LongInt;                       {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetServerName :widestring;              {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetServiceName :WideString;             {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetAutoCreate(aValue :Boolean);        {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetDataDirectory(aValue :widestring);  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetDefaultDBName(aValue :widestring);  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetDefaultExt(aValue :Widestring);     {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetPort(aValue :LongInt);              {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetServerName(aValue :widestring);     {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetServiceNAme(aValue :WideString);    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    property DataDirectory :widestring read GetDataDirectory write SetDataDirectory; //the default data directory where the catalogs will be placed. This might be a directory that only the server application has access
    property DefaultDBName :widestring read GetDefaultDBName write SetDefaultDBName; //Which is the default catalog to open at this time this should be db1
    property DefaultExt    :Widestring read GetDefaultExt    write SetDefaultExt;    //Default extension will be used only from a open dialog.
    property AutoCreate    :Boolean    read GetAutoCreate    write SetAutoCreate;    //If the default catalog is not found should it be auto created?
    property Port          :LongInt    read GetPort          write SetPort;          //port to listen for incoming connections
    property ServiceName   :WideString read GetServiceName   write SetServiceNAme;   //Returns ThinkSQL
    property ServerName    :widestring read GetServerName    write SetServerName;    //Returns ThinkSQL
    //extend it to support
    //1) which network cards to attach it self to.
    //2) from which ip addresses/ranges it will accept connection from
    //3) how to respond to unauthorised IPs
    //4) More Database defaults like default isolation level or collation etc.
    // this is to be considered an internal interface only. Its not to be used from external utilities
    // for that a more sql centric approach is needed.
  end;

function Options:IEvsThsRuntimeOptions;

implementation
uses syncobjs, IniFiles, {uEvsEncoding} IdCoderUUE; //do not polute the code with more external requirements.
type

  { TEvsInterfacedObject }
  {This is a simple IUnknown that can thake the role of either an aggregate or a stand alone interface}
  //move it to a "base" unit so everyone can use it.
  TEvsInterfacedObject = class(TObject,IUnknown)
  protected
    FRefCount : Longint;
    FOwner    : Pointer;
    { implement methods of IUnknown }
    function QueryInterface(constref Iid : TGuid;out Obj) : Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : Longint;                                     {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : Longint;                                    {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    constructor Create(const aOwner: IUnknown=nil);virtual;
    procedure AfterConstruction;override;
    procedure BeforeDestruction;override;
    class function NewInstance : TObject;override;
    property RefCount : Longint read FRefCount;
  end;

  { TEvsIniPersister }
  //for now this is the default settings file. move it to the next
  TEvsIniPersister = class(TEvsInterfacedObject, IEvsOptionPersister)
  private
    FFile    :TIniFile;
    FSection :String;
    FLock    :TSynchroObject;
    function GetSection :string;
    procedure SetSection(aValue :string);
  public
    procedure Lock;
    procedure Unlock;
    constructor Create(const aOwner :IUnknown =nil); override;overload;
    constructor Create(const aFilename:String;const aOwner :IUnknown =nil); overload;
    procedure Setfilename(const aFilename:string);
    procedure Write        (const aKey:WideString; const aBuffer; const aSize:integer); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    procedure WriteString  (const aKey, aValue:WideString);                                 {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteInteger (const aKey:WideString; const aValue:Integer);                   {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteDouble  (const aKey:WideString; const aValue:Double);                    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteDateTime(const aKey:WideString; const aValue:TDateTime);                 {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteInt64   (const aKey:WideString; const aValue: Int64);                    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure WriteBoolean (const aKey:WideString; const aValue: Boolean);                  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    function ReadString   (const aKey, aDefaultValue:WideString):wideString;                {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadInteger  (const aKey:WideString;const aDefaultValue:Integer)  :Integer;    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadDouble   (const aKey:WideString;const aDefaultValue:Double)   :Double;     {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadDateTime (const aKey:WideString;const aDefaultValue:TDateTime):TDateTime;  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadInt64    (const aKey:WideString;const aDefaultValue:Int64)    : Int64;     {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function ReadBoolean  (const aKey:WideString;const aDefaultValue:Boolean)  : Boolean;   {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    function Read         (const aKey:WideString; var aBuffer; const aBufSize:integer):Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    procedure Load(const aSubject:IEvsPersistable);                                         {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure Save(const aSubject:IEvsPersistable);                                         {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    property Section:string read GetSection write SetSection;
  end;

  { TEvsThsOptions }
  //this control should never be moved to the interface part otherwise it will require a singleton implementation
  TEvsThsOptions = class(TEvsInterfacedObject, IEvsThsRuntimeOptions, IEvsPersistable)
  private
    FLock         :TCriticalSection;
    FAutoCreate   :Boolean;
    FDataFolder   :WideString;
    FDefaultDB    :WideString;
    FDefaultExt   :WideString;
    FPort         :LongInt;
    FServiceName  :WideString;
    FServerName   :WideString;
  public
    constructor Create(const aOwner: IUnknown=nil);override;
    Procedure Lock;
    Procedure Unlock;
    function GetAutoCreate :Boolean;                   {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetDataDirectory :widestring;             {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetDefaultDBName :widestring;             {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetDefaultExt    :Widestring;             {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetPort          :LongInt;                {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetServiceName   :widestring;             {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function GetServerName :widestring;                {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetAutoCreate   (aValue :Boolean);       {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetDataDirectory(aValue :widestring);    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetDefaultDBName(aValue :widestring);    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetDefaultExt   (aValue :Widestring);    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetPort(aValue :LongInt);                {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetServiceNAme(aValue :widestring);      {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure SetServerName(aValue :widestring);       {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    procedure SaveTo(const aPersister: IEvsOptionPersister);    {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    procedure LoadFrom(const aPersister:IEvsOptionPersister);  {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    property DataDirectory :WideString read GetDataDirectory write SetDataDirectory; //the default data directory where the catalogs will be placed. This might be a directory that only the server application has access
    property DefaultDBName :WideString read GetDefaultDBName write SetDefaultDBName; //Which is the default catalog to open at this time this should be db1
    property DefaultExt    :WideString read GetDefaultExt    write SetDefaultExt;
    property AutoCreate    :Boolean    read GetAutoCreate    write SetAutoCreate;    //If the default catalog is not found should it be auto created?

    property Port          :LongInt    read GetPort          write SetPort;          //at which port listen for incoming connections
    property ServiceName   :widestring read GetServiceName   write SetServiceName;   // Returns ThinkSQL
    property ServerName    :widestring read GetServerName    write SetServerName;   // Returns ThinkSQL
  end;

{$REGION ' TEvsIniPersister '}

function TEvsIniPersister.GetSection :string;
begin
  Lock;
  try
    Result := FSection;
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.SetSection(aValue :string);
begin
  Lock;
  try
    FSection := aValue;
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.Lock;
begin
  FLock.Acquire;
end;

procedure TEvsIniPersister.Unlock;
begin
  FLock.Release;
end;

constructor TEvsIniPersister.Create(const aOwner :IUnknown);
begin
  inherited Create(aOwner);
  FSection := 'Main';
  FLock := TCriticalSection.Create;
end;

constructor TEvsIniPersister.Create(const aFilename :String;
  const aOwner :IUnknown);
begin
  Create(aOwner);
  FFile := TIniFile.Create(aFilename);
end;

procedure TEvsIniPersister.Setfilename(const aFilename :string);
begin
  if Assigned(FFile) then FreeAndNil(FFile);
  FFile := TIniFile.Create(aFilename);
end;

procedure TEvsIniPersister.WriteString(const aKey, aValue :WideString); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FFile.WriteString(FSection, aKey, aValue);
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.WriteInteger(const aKey :WideString;const aValue :Integer); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FFile.WriteInteger(FSection, aKey, aValue);
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.WriteDouble(const aKey :WideString; const aValue :Double); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FFile.WriteFloat(FSection, aKey, aValue);
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.WriteDateTime(const aKey :WideString;const aValue :TDateTime); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FFile.WriteDateTime(FSection, aKey, aValue);
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.WriteInt64(const aKey :WideString;const aValue :Int64); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FFile.WriteInt64(FSection, aKey, aValue);
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.WriteBoolean(const aKey :WideString;const aValue :Boolean); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FFile.WriteBool(FSection, aKey, aValue);
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.Write(const aKey :WideString; const aBuffer; const aSize :integer); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
  vMStrm :TMemoryStream;
  vEnc  :TIdEncoderUUE;
  vSStrm :TStringSTream;
begin
  Lock;
  try
    vMStrm := TMemoryStream.Create;
    vEnc   := TIdEncoderUUE.Create;
    vSStrm := TStringStream.Create('');
    try
      vMStrm.Write(aBuffer, aSize);
      vEnc.Encode(vMstrm, vSStrm);
      FFile.WriteString(FSection, aKey, vSStrm.DataString);
    finally
      vSStrm.Free;
      vEnc.Free;
      vMStrm.Free;
    end;
  finally
    Unlock;
  end;
end;

function TEvsIniPersister.ReadString(const aKey, aDefaultValue :WideString) :WideString; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FFile.ReadString(FSection, aKey, aDefaultValue);
  finally
    Unlock;
  end;
end;

function TEvsIniPersister.ReadInteger(const aKey :Widestring;const aDefaultValue:Integer) :Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FFile.ReadInteger(fSection,aKey,aDefaultValue);
  finally
    Unlock;
  end;
end;

function TEvsIniPersister.ReadDouble(const aKey :WideString;const aDefaultValue:Double) :Double; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FFile.ReadFloat(FSection, aKey, aDefaultValue);
  finally
    Unlock;
  end;
end;

function TEvsIniPersister.ReadDateTime(const aKey :WideString;const aDefaultValue:TDateTime) :TDateTime; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FFile.ReadDateTime(FSection,aKey,aDefaultValue);
  finally
    Unlock;
  end;
end;
function TEvsIniPersister.ReadInt64(const aKey :WideString;const aDefaultValue:Int64) :Int64; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FFile.ReadInt64(FSection, aKey, aDefaultValue);
  finally
    Unlock;
  end;
end;
function TEvsIniPersister.ReadBoolean(const aKey :WideString;const aDefaultValue:Boolean) :Boolean; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FFile.ReadBool(FSection,aKey,aDefaultValue);
  finally
    Unlock;
  end;
end;

function TEvsIniPersister.Read(const aKey :wideString; var aBuffer;const aBufSize :integer) :Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
//returns the size of the decoded data in bytes if the size is smaller
//or equal to abufsize then it copies the decoded resul to the abuffer
//otherwise it does not.
var
  vMStrm :TMemoryStream;
  vDec   :TIdDecoderUUE;
  vSStrm :TStringSTream;
  vData  :string;
  vBytes :TIDBytes;
begin
  Lock;
  try
    vMStrm := TMemoryStream.Create;
    vDec   := TIdDecoderUUE.Create;
    vSStrm := TStringStream.Create('');
    try
      vData := FFile.ReadString(FSection, aKey, '');
      if vData<>'' then begin
        vBytes := vDec.DecodeBytes(vData);
        Result := Length(vBytes);
        if aBufSize >= Result then Move(vBytes[0],aBuffer,Length(vBytes));
      end;
    finally
      vSStrm.Free;
      vDec.Free;
      vMStrm.Free;
    end;
  finally
    Unlock;
  end;
end;

procedure TEvsIniPersister.Load(const aSubject :IEvsPersistable);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  aSubject.LoadFrom(Self);
end;

procedure TEvsIniPersister.Save(const aSubject :IEvsPersistable); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  aSubject.SaveTo(Self);
end;

{$ENDREGION}

{$REGION 'TEvsInterfacedObject' }
function TEvsInterfacedObject.QueryInterface( constref Iid : TGuid;out Obj) : Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
if GetInterface(iid,obj) then result:=S_OK
else result:=longint(E_NOINTERFACE);
end;

function TEvsInterfacedObject._AddRef :Longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if Assigned(FOwner) then Result := IUnknown(FOwner)._AddRef
  else Result := InterLockedIncrement(FRefCount);
end;

function TEvsInterfacedObject._Release :Longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if Assigned(FOwner) then
    Result := IUnknown(FOwner)._Release
  else begin
    Result := InterLockedDecrement(FRefCount);
    if Result = 0 then Destroy;
  end;
end;

constructor TEvsInterfacedObject.Create(const aOwner :IUnknown=nil);
begin
  inherited Create;
  FOwner := Pointer(aOwner);
end;

procedure TEvsInterfacedObject.AfterConstruction;
begin
   {we need to fix the refcount we forced in newinstance}
   {further, it must be done in a thread safe way       }
   InterLockedDecrement(FRefCount);
end;

procedure TEvsInterfacedObject.BeforeDestruction;
begin
  if not Assigned(FOwner) then if (FRefCount<>0) then
      raise Exception.Create('Unable to destroy '+ClassName+'. There are more references on this object.');
end;

class function TEvsInterfacedObject.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  if Result<>nil then TEvsInterfacedObject(Result).FRefCount:=1;
end;

{$ENDREGION}

{$REGION ' TEvsThsOptions '}

function TEvsThsOptions.GetServiceName :widestring; stdcall;
begin
  Lock;
  try
    Result := FServiceName;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SetServiceNAme(aValue :widestring); stdcall;
begin
  Lock;
  try
    FServiceName := aValue;
  finally
    Unlock;
  end;
end;

function TEvsThsOptions.GetServerName :widestring;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FServerName;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SetServerName(aValue :widestring);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FServerName := aValue;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SaveTo(const aPersister :IEvsOptionPersister); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    aPersister.WriteString ('DatabaseName',     FDefaultDB);
    aPersister.WriteString ('DefaultExtension', DefaultExt);
    aPersister.WriteString ('DataFolder',       FDataFolder);
    aPersister.WriteString ('ServiceName',      FServiceName);
    aPersister.WriteString ('ServerName',       FServerName);
    aPersister.WriteInteger('Port',             FPort);
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.LoadFrom(const aPersister :IEvsOptionPersister);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FDefaultDB   := aPersister.ReadString ('DatabaseName', 'db1');
    FDefaultExt  := aPersister.ReadString ('DefaultExtension',uGlobal.DB_FILE_EXTENSION);//or something else?
    FDataFolder  := aPersister.ReadString ('DataFolder', IncludeTrailingPathDelimiter(GetUserDir)+'ThinkSQL_Data');
    FServiceName := aPersister.ReadString ('ServiceName','ThinkSql');
    FServerName  := aPersister.ReadString ('ServerName', 'ThinkSQL');
    FPort        := aPersister.ReadInteger('Port', 9075);
  finally
    Unlock;
  end;
end;

constructor TEvsThsOptions.Create(const aOwner :IUnknown);
begin
  inherited;
  FAutoCreate  := False;
  FDataFolder  := '';
  FDefaultDB   := 'db1';
  FServiceName := 'ThinkSql';
  FPort        := 9075;
  FServerName  := 'ThinkSql';
  FLock := TCriticalSection.Create;
end;

Procedure TEvsThsOptions.Lock;
begin
  FLock.Acquire;
end;

Procedure TEvsThsOptions.Unlock;
begin
  FLock.Release;
end;

function TEvsThsOptions.GetAutoCreate :Boolean; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FAutoCreate;
  finally
    Unlock;
  end;
end;

function TEvsThsOptions.GetDataDirectory :widestring; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FDataFolder;
  finally
    Unlock;
  end;
end;

function TEvsThsOptions.GetDefaultDBName :widestring;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FDefaultDB;
  finally
    Unlock;
  end;
end;

function TEvsThsOptions.GetDefaultExt :Widestring;{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    Result := FDefaultExt;
  finally
    Unlock;
  end;
end;

function TEvsThsOptions.GetPort :LongInt; stdcall;
begin
  Lock;
  try
    Result := FPort;;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SetAutoCreate(aValue :Boolean); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FAutoCreate := aValue;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SetDataDirectory(aValue :widestring);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FDataFolder := aValue;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SetDefaultDBName(aValue :widestring);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FDefaultDB := aValue;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SetDefaultExt(aValue :Widestring);{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Lock;
  try
    FDefaultExt := aValue;
  finally
    Unlock;
  end;
end;

procedure TEvsThsOptions.SetPort(aValue :LongInt); stdcall;
begin
  Lock;
  try
    FPort := aValue;
  finally
    Unlock;
  end;
end;

{$ENDREGION}


var
  vOptions:IEvsThsRuntimeOptions;
{
This function does two things
1) it returns a config file name that is unique for the application its called from.
2) it uses OS specific directories for the settings file unless it finds one in the same
   directory as the executable in which case it returns that config filename for portable
   use.
It makes sure that the filename returned has a complete path to avoid using the OS's current
directory and remove any ambiquity from the process.
}
function GetConfigFilename:string;
begin
  Result := ChangeFileExt(ParamStr(0),'.cfg');
  if not FileExists(Result) then begin
    Result := IncludeTrailingPathDelimiter(GetAppConfigDir(True))+ExtractFileName(ParamStr(0));
    Result := ChangeFileExt(Result,'.cfg');
  end;
end;

function Options:IEvsThsRuntimeOptions;
  procedure LoadDefaults;
  var
    IniFile : IEvsOptionPersister;
    CfgFilename: String;
  begin
    CfgFilename := GetConfigFilename;
    if FileExists(CfgFilename) then begin
      IniFile := TEvsIniPersister.Create(CfgFilename,nil);
      IniFile.Load(vOptions);
      IniFile := Nil;
    end;
  end;
begin
  if not Assigned(vOptions) then begin
    vOptions := TEvsThsOptions.Create;
    LoadDefaults;
  end;
  Result := vOptions;
end;

function DoVendorName:String;
begin
  Result := 'Evosi';
end;

initialization
  vOptions := Nil; //no random data please
  OnGetVendorName := DoVendorName;

finalization
  vOptions := Nil; //clean up.

end.

