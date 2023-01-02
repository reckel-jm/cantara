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
  NextFileName: String;
begin
  NextFileName := songtexfile.HasNextSongfile;
  AssertEquals('Amazing Grace.song', NextFileName);
  AssertEquals('1. Amazing grace', SongTexFile.NextSongFile.Strings[0]);

  NextFileName := songtexfile.HasNextSongfile;
  AssertEquals('And Can It Be.song', NextFileName);
  AssertEquals('#title: And Can It Be', SongTexFile.NextSongFile.Strings[0]);

  NextFileName := songtexfile.HasNextSongfile;
  AssertEquals('', NextFileName);

  NextFileName := songtexfile.HasNextSongfile;
  AssertEquals('', NextFileName);

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

