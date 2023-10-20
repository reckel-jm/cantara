unit filehandlertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TCantaraCLITest= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TCantaraCLITest.TestHookUp;
begin
  Fail('Write your own test');
end;



initialization

  RegisterTest(TCantaraCLITest);
end.

