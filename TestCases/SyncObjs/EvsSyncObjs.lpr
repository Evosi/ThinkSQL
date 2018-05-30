program EvsSyncObjs;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner, TestSeamphores, TestEvsSyncObjs;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

