unit contentselectionmodeltest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testconstants,
  contentselectionmodel;

type

  { TContentRepositoryModalTest }

  TContentRepositoryModalTest= class(TTestCase)
  private
    RepositoryModel: TRepositoryModel;
  published
    procedure TestRepoImport;
    procedure TestRepoFile;
    procedure TestUnvalidFile;
  end;

implementation

procedure TContentRepositoryModalTest.TestRepoImport;
var ContentFile: TContentFile;
begin
  RepositoryModel := TRepositoryModel.Create;
  RepositoryModel.LoadRepository(TestConstants.TestDataDirectory);
  AssertTrue('Number of items in Song Repository not correct! There are ' +
                     IntToStr(RepositoryModel.ItemCount), RepositoryModel.ItemCount = 5);
  for ContentFile in RepositoryModel.RepositoryFiles do
    WriteLn(ContentFile.DisplayName);
  RepositoryModel.Destroy;
end;

procedure TContentRepositoryModalTest.TestRepoFile;
var ARepoFile: TContentFile;
  TestFile, GivenFilePath: String;
  TestFiles: array of String;
begin
  TestFiles := ['ExampleCCLISong1.ccli', 'Weiß ich den Weg auch nicht.ccli'];
  for TestFile in TestFiles do
  begin
    GivenFilePath := TestConstants.TestDataDirectory + PathDelim +
    'ExampleCCLISong1.ccli';
    ARepoFile := TContentFile.Create(GivenFilePath);
    AssertTrue('Display Namei is ' + ARepoFile.DisplayName,
                        ARepoFile.DisplayName='ExampleCCLISong1');
    AssertTrue('File Path is ' + ARepoFile.FilePath,
                        ARepoFile.FilePath=GivenFilePath);
    AssertTrue('File has not been recognised as song.', ARepoFile.ContentType=TContentType.Song);
    AssertTrue('File is not valid.', ARepoFile.Valid);
    ARepoFile.Destroy;
  end;
end;

procedure TContentRepositoryModalTest.TestUnvalidFile;
var ARepoFile: TContentFile;
  TestFile, GivenFilePath: String;
  TestFiles: array of String;
begin
  TestFiles := ['A Not Existing File.ccli', 'test01.songtex'];
  for TestFile in TestFiles do
  begin
    GivenFilePath := TestConstants.TestDataDirectory + PathDelim +
    'ExampleCCLISong1.ccli';
    ARepoFile := TContentFile.Create(GivenFilePath);
    AssertTrue('File ' + TestFile + ' has been wrongly recognized as valid.',
                     ARepoFile.Valid);
    ARepoFile.Destroy;
  end;
end;



initialization

  RegisterTest(TContentRepositoryModalTest);
end.

