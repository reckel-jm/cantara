unit lyricstests;

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
    procedure TestTagRecognitionFromSongFormatFiles;
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
  try
    FindAllFiles(CCLISongFiles, TestDataDirectory, '*.ccli', True);
    for CCLIFileName in CCLISongFiles do
    begin
      try
        TestSong := TSong.Create;
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
      finally
        TestSong.Destroy;
      end;
    end;
  finally
    CCLISongConvert.Destroy;
    SongFormatSong.Destroy;
    CCLISongFiles.Destroy;
  end;
end;

procedure TCCLIFileConversionTest.TestTagRecognitionFromSongFormatFiles;
var TestSong: Lyrics.TSong;
begin
  TestSong := TSong.Create;
  try
    TestSong.importSongfile(TestDataDirectory + PathDelim + 'Oh, What a Savior that He Died For Me.song');
    AssertTrue('Author Tag has not been recognised',
                       Testsong.MetaDict.KeyData['author'] = 'James McGranahan');
    AssertTrue('Title Tag has not been recognised correctly from filename',
                       Testsong.MetaDict.KeyData['title'] = 'Oh, What a Savior that He Died For Me');
  finally
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

