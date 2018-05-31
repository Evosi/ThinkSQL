program EvsSyncObjs;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner, TestSeamphores, TestEvsMutex;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

