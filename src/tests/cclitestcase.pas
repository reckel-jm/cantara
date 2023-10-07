unit cclitestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Slides, Lyrics,
  FileUtil;

type

  { TCCLIFileConversionTest }

  TCCLIFileConversionTest= class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestCCLIToSongFormatConversionIsCorrect;
    procedure TearDown; override;
  end;

const
  TestDataDirectory:String = 'testdata';

implementation

procedure TCCLIFileConversionTest.TestCCLIToSongFormatConversionIsCorrect;
var
  CCLISongFiles, CCLISongConvert, SongFormatSong: TStringList;
  CCLIFileName: String;
  SongFormatFileName: String;
  TestSong: Lyrics.TSong;
begin
  CCLISongFiles := TStringList.Create;
  CCLISongConvert := TStringList.Create;
  SongFormatSong := TStringList.Create;
  TestSong := TSong.Create;
  try
    FindAllFiles(CCLISongFiles, TestDataDirectory, '*.ccli', True);
    for CCLIFileName in CCLISongFiles do
    begin
      TestSong.importSongfile(CCLIFileName);
      CCLISongConvert.Text := TestSong.exportAsSongFile;
      SongFormatFileName := StringReplace(CCLIFileName, '.ccli', '.song', [rfReplaceAll]);
      if not FileExists(SongFormatFileName) then
        Fail(Format('The file "%s" does not exist.', [SongFormatFileName]));
      SongFormatSong.LoadFromFile(SongFormatFileName);
      //AssertTrue(CCLIFileName, Trim(CCLISongConvert.Text) = Trim(SongFormatSong.Text));
      if Trim(CCLISongConvert.Text) <> Trim(SongFormatSong.Text) then
         Fail('The converted output of ' + CCLIFileName + ' and ' + SongFormatFileName +
         ' is not equal.' + LineEnding + 'This is the output of the conversion: '
         + LineEnding + CCLISongConvert.Text);
    end;
  finally
    CCLISongConvert.Destroy;
    SongFormatSong.Destroy;
    CCLISongFiles.Destroy;
    TestSong.Destroy;
  end;
end;

procedure TCCLIFileConversionTest.TearDown;
begin
  inherited TearDown;
end;

procedure TCCLIFileConversionTest.SetUp;
begin
end;


initialization

  RegisterTest(TCCLIFileConversionTest);
end.

