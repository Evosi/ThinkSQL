unit TestLightSwitch;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, sysutils, windows, uEvsSyncObjs;

type
  // Test methods for class TEvsLightSwitch
  
  TestTEvsLightSwitch = class(TTestCase)
  strict private
    FSwitch: TEvsLightSwitch;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTurnOn;
    procedure TestTurnOff;
  end;

implementation

procedure TestTEvsLightSwitch.SetUp;
begin
  FSwitch := TEvsLightSwitch.Create;
end;

procedure TestTEvsLightSwitch.TearDown;
begin
  FSwitch.Free;
  FSwitch := nil;
end;

procedure TestTEvsLightSwitch.TestTurnOn;
begin
  CheckFalse(FSwitch.IsOn,'Its not initialized correctly');
  FSwitch.TurnOn;
  CheckTrue(FSwitch.IsOn,'Is this thing on or what?');
  FSwitch.TurnOn;
  FSwitch.TurnOn;
  FSwitch.TurnOn;
  FSwitch.TurnOn;
  FSwitch.TurnOn;
  FSwitch.TurnOn;
  CheckTrue(FSwitch.IsOn,'Oh come on some one check the braker!');
end;

procedure TestTEvsLightSwitch.TestTurnOff;
begin
  CheckFalse(FSwitch.IsOn,'Its not initialized correctly');
  FSwitch.TurnOn;
  CheckTrue(FSwitch.IsOn,'Is this thing on or what?');
  FSwitch.TurnOn;
  CheckTrue(FSwitch.IsOn,'Is this thing on or what?');
  FSwitch.TurnOn;
  CheckTrue(FSwitch.IsOn,'Oh come on some one check the braker!');

  FSwitch.TurnOff;
  CheckTrue(FSwitch.IsOn,'Is this thing on or what?');
  FSwitch.TurnOff;
  CheckTrue(FSwitch.IsOn,'Is this thing on or what?');
  FSwitch.TurnOff;
  CheckFalse(FSwitch.IsOn,'Is this thing on or what?');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTEvsLightSwitch.Suite);
end.

