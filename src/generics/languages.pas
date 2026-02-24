unit languages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function IsRTLLanguage(const AText: string): Boolean;


implementation

function IsRTLLanguage(const AText: string): Boolean;
var
  U: UnicodeString;
  i: Integer;
begin
  Result := False;
  U := UTF8Decode(AText);
  for i := 1 to Length(U) do
  begin
    // Unicode ranges for RTL scripts:
    // Hebrew: $0590..$05FF
    // Arabic/Farsi/Urdu: $0600..$06FF, $0750..$077F, $08A0..$08FF
    if (Word(U[i]) >= $0590) and (Word(U[i]) <= $08FF) then
    begin
      Exit(True);
    end;
  end;
end;

end.

