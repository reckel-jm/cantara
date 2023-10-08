program fpccantaraclitest;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, LyricsTests, MultiLanguage;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Cantara Console Test Runner';
  Application.Run;
  Application.Free;
end.
