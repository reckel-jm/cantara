unit multilanguage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Lyrics, Slides, Languages;

type

  TTestBIDIDetection= class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestHookUp;
    procedure TestWesternTextWithPunctuation;
    procedure TestHebrewDetection;
    procedure TestArabicDetection;
    procedure TestMixedNeutralCharacters;
    procedure TestChineseLetters;
    procedure TestEmptyString;
  end;

implementation

procedure TTestBiDiDetection.TestWesternTextWithPunctuation;
var
  Input: string;
begin
  Input := '!Jesus, du allein. Hey!';
  // AssertEquals(ErrorMessage, ExpectedValue, ActualValue)
  AssertEquals('Western text with leading punctuation should NOT be RTL',
               False, IsRTLLanguage(Input));
end;

procedure TTestBiDiDetection.TestHebrewDetection;
begin
  if not IsRTLLanguage('שלום') then
    Fail('Failed to detect Hebrew script as RTL');
end;

procedure TTestBiDiDetection.TestArabicDetection;
begin
  if not IsRTLLanguage('العربية') then
    Fail('Failed to detect Arabic script as RTL');
end;

procedure TTestBiDiDetection.TestMixedNeutralCharacters;
begin
  // Standard symbols and numbers should be LTR by default
  AssertEquals('Numbers and symbols should stay LTR',
               False, IsRTLLanguage('1234567890 !!! ???'));
end;

procedure TTestBiDiDetection.TestEmptyString;
begin
  AssertEquals('Empty string should default to LTR',
               False, IsRTLLanguage(''));
end;

procedure TTestBiDiDetection.TestChineseLetters;
begin
  // Standard symbols and numbers should be LTR by default
  AssertEquals('Chinese letters should stay LTR',
               False, IsRTLLanguage('你好！你今天怎么样？'));
end;

procedure TTestBIDIDetection.TestHookUp;
begin

end;

procedure TTestBIDIDetection.SetUp;
begin

end;


initialization

  RegisterTest(TTestBIDIDetection);
end.

