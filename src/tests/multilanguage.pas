unit multilanguage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Lyrics, Slides;

type

  TSplitMultiLanguage= class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TSplitMultiLanguage.TestHookUp;
begin
  Fail('Write your own test');
end;

procedure TSplitMultiLanguage.SetUp;
begin

end;


initialization

  RegisterTest(TSplitMultiLanguage);
end.

