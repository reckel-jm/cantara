unit languages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

{ Checks whether a String consists of a right-to-left language (Hebrew, Arabic or Farsi.
Returns true if the language is right-to-left, returns false if the language is left-to-right. }
function IsRTLLanguage(const AText: string): Boolean;


implementation

function IsRTLLanguage(const AText: string): Boolean;
var
  U: UnicodeString;
  i: Integer;
  Code: Word;
begin
  Result := False;
  if AText = '' then Exit;

  // Convert UTF-8 byte-string to a UTF-16 UnicodeString
  U := UTF8Decode(AText);

  // Now Length(U) returns the actual number of characters
  for i := 1 to Length(U) do
  begin
    Code := Word(U[i]);

    // Range Check:
    // $0590-$05FF: Hebrew
    // $0600-$06FF: Arabic
    // $0750-$077F: Arabic Supplement
    // $08A0-$08FF: Arabic Extended
    if (Code >= $0590) and (Code <= $08FF) then
    begin
      Exit(True);
    end;
  end;
end;

end.

