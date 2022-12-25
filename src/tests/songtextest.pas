unit songtextest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, SongTeX;

type

  TSongTeXTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;
var
  songtexfile: TSongTeXFile;

implementation

procedure TSongTeXTest.TestHookUp;
var
  AmazingGrace, AndCanItBe: String;
begin
  AmazingGrace := songtexfile.HasNextSongfile;
  AssertEquals('Amazing Grace.song', AmazingGrace);
  AssertEquals('1. Amazing grace', SongTexFile.NextSongFile.Strings[0]);

  AndCanItBe := songtexfile.HasNextSongfile;
  AssertEquals('And Can It Be.song', AndCanItBe);
  AssertEquals('#title: And Can It Be', SongTexFile.NextSongFile.Strings[0]);

  FreeAndNil(SongTeXFile);
end;

procedure TSongTeXTest.SetUp;
begin
  songtexfile := TSongTeXFile.Create;
  songtexfile.LoadFromFile('tests' + PathDelim + 'test01.songtex');
end;

procedure TSongTeXTest.TearDown;
begin
  if Assigned(songtexfile) then FreeAndNil(SongTeXFile);
end;


initialization

  RegisterTest(TSongTeXTest);
end.

