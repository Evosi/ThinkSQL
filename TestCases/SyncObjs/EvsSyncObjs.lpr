program EvsSyncObjs;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner,
  TestSeamphores,
  TestEvsMutex,
  TestMREWSynch,
  TestCriticalSection, uEvsThreads;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

