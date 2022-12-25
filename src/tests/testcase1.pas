unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TCantaraTest= class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TCantaraTest.TestHookUp;
begin

end;

procedure TCantaraTest.SetUp;
begin

end;


initialization

  RegisterTest(TCantaraTest);
end.

