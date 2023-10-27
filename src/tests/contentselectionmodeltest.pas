unit contentselectionmodeltest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testconstants,
  contentselectionmodel, CantaraContentFile;

type

  { TContentRepositoryModalTest }

  TContentRepositoryModalTest= class(TTestCase)
  private

  published
    procedure TestRepoImport;
    procedure TestRepoFile;
    procedure TestUnvalidFile;
    procedure TestSelectFile;
    procedure TestSongTexExport;
  end;

implementation

procedure TContentRepositoryModalTest.TestRepoImport;
var ContentFile: TContentFile;
  RepositoryModel: TRepositoryModel;
  i: Integer;
begin
  RepositoryModel := TRepositoryModel.Create;
  RepositoryModel.LoadRepository(TestConstants.TestDataDirectory);
  AssertTrue('Number of items in Song Repository not correct! There are ' +
                     IntToStr(RepositoryModel.ItemCount), RepositoryModel.ItemCount = TestConstants.NumberOfSongFiles);
  for i := 0 to RepositoryModel.ItemCount-1 do
    WriteLn(RepositoryModel.RepositoryFiles.Items[i].FilePath);
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
    AssertTrue('Display Name is ' + ARepoFile.DisplayName,
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

procedure TContentRepositoryModalTest.TestSelectFile;
var ARepoFile: TContentFile;
  RepositoryModel: TRepositoryModel;
  ATestFile: TContentFile;
  SelectionModel: TContentSelectionModel;
begin
  RepositoryModel := TRepositoryModel.Create;
  RepositoryModel.LoadRepository(TestConstants.TestDataDirectory);
  ATestFile := RepositoryModel.RepositoryFiles.Items[0];
  SelectionModel := TContentSelectionModel.Create;
  SelectionModel.DefaultPresentationStyleSettings.Transparency:=30;
  SelectionModel.SelectFile(ATestFile);
  WriteLn('Testfile created.');
  AssertTrue('The Selected file has not the default transparancy',
                  SelectionModel.SelectionList.Items[0].PresentationStyleSettings.Transparency=30);
  SelectionModel.Destroy;
  AssertTrue(ATestFile.Valid);
  RepositoryModel.Destroy;
end;

procedure TContentRepositoryModalTest.TestSongTexExport;
var RepositoryModel: TRepositoryModel;
begin
  RepositoryModel := TRepositoryModel.Create;
  RepositoryModel.LoadRepository(TestConstants.TestDataDirectory);
end;


initialization

  RegisterTest(TContentRepositoryModalTest);
end.

