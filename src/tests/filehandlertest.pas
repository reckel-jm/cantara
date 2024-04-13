unit filehandlertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, textfilehandler,
  TestConstants;

type

  { TCantaraCLITest }

  TCantaraCLITest= class(TTestCase)
  private
  protected
    TextFileHandler: TTextFileHandler;
    procedure SetUp; override;
  published
    procedure TestOpenFile;
    procedure TestOpenNonExistingFile;
    procedure TestResetFile;
    procedure TestSaveFileWithoutEnding;
    procedure TestSaveFileWithFileEnding;
    procedure TearDown; override;
  end;

implementation

procedure TCantaraCLITest.SetUp;
begin
  inherited SetUp;
  Self.TextFileHandler := TTextFileHandler.Create;
end;

procedure TCantaraCLITest.TestOpenFile;
var TextFileString: String;
begin
  TextFileString := Self.TextFileHandler.OpenTextFile(TestConstants.TestDataDirectory + PathDelim
                               + 'Oh, What a Savior that He Died For Me.song');
  AssertTrue('The Status of TextFileHandler is Wrong.',
                  TextFileHandler.FileHandlingStatus = TFileHandlingStatus.StatusSuccess);
  AssertTrue('The opened TestFile is empty but should not.',  TextFileString <> '');
end;

procedure TCantaraCLITest.TestOpenNonExistingFile;
var TextFileString: String;
begin
  TextFileString := Self.TextFileHandler.OpenTextFile('a non existing filepath' + PathDelim + 'a non existing file');
  AssertTrue(self.TextFileHandler.FileHandlingStatus = TFileHandlingStatus.StatusErrorFileDoesNotExist);
end;

procedure TCantaraCLITest.TestResetFile;
begin
  Self.TextFileHandler.Reset;
  AssertTrue('Error Message has not been resetted correctly',
                     Self.TextFileHandler.ErrorMessage='');
  AssertTrue('Status has not been resetted correctly.',
                     Self.TextFileHandler.FileHandlingStatus=TFileHandlingStatus.StatusInactive);
end;

procedure TCantaraCLITest.TestSaveFileWithoutEnding;
var TextFileString, TextFilePath: String;
  TextFileStringList: TStringList;
begin
  TextFileString := 'Here is a test file string';
  TextFilePath   := TestDataDirectory + PathDelim + 'testsongtest01';
  TextFileHandler.SaveTextFile(TextFileString, TextFilePath, '.song');
  AssertTrue('Status of TTextFileHandler is not success.',
                     TextFileHandler.FileHandlingStatus = TFileHandlingStatus.StatusSuccess);
  AssertTrue('File Ending ist not .song', ExtractFileExt(TextFilePath) = '.song');
  AssertTrue('The saved file does not exist.', FileExists(TextFilePath));
  TextFileStringList := TStringList.Create;
  try
    TextFileStringList.LoadFromFile(TextFilePath);
    AssertTrue('Content of the file has not been saved.', Trim(TextFileStringList.Text) = Trim(TextFileString));
  finally
    TextFileStringList.Destroy;
  end;
end;

procedure TCantaraCLITest.TestSaveFileWithFileEnding;
var TextFileString, TextFilePath: String;
  TextFileStringList: TStringList;
begin
  TextFileString := 'Here is a test file string';
  TextFilePath   := TestDataDirectory + PathDelim + 'testsongtest02.song';
  TextFileHandler.SaveTextFile(TextFileString, TextFilePath, '.song');
  AssertTrue('Status of TTextFileHandler is not success.',
                     TextFileHandler.FileHandlingStatus = TFileHandlingStatus.StatusSuccess);
  AssertTrue('File path has been changed but shouldn''t', TextFilePath = TestDataDirectory + PathDelim + 'testsongtest02.song');
  AssertTrue('The saved file does not exist.', FileExists(TextFilePath));
  TextFileStringList := TStringList.Create;
  try
    TextFileStringList.LoadFromFile(TextFilePath);
    AssertTrue('Content of the file has not been saved.', Trim(TextFileStringList.Text) = Trim(TextFileString));
  finally
    TextFileStringList.Destroy;
  end;
end;

procedure TCantaraCLITest.TearDown;
var CleanUpFiles: array of String;
  CleanUpFile: String;
begin
  inherited TearDown;
  CleanUpFiles := ['testsongtest01.song', 'testsongtest02.song'];
  for CleanUpFile in CleanUpFiles do
      if FileExists(TestDataDirectory + PathDelim + CleanUpFile) then
         DeleteFile(TestDataDirectory + PathDelim + CleanUpFile);
end;



initialization

  RegisterTest(TCantaraCLITest);
end.

