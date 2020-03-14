program SyncObjectsTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner,
  TestSeamphores,
  TestEvsMutex,
  TestMREWSynch,
  TestCriticalSection, TestThreadLocalCount, UMREWSDLL, UTestDelphiMREWS;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

