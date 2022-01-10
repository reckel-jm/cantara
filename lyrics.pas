unit lyrics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function StringListToString(StringList: TStringList): String;

implementation

function StringListToString(StringList: TStringList): String;
var s: String; i: integer;
begin
  s := '';
  if StringList.Count < 1 then exit(s);
  for i := 0 to StringList.Count-2 do
  begin
    s := s + StringList.Strings[i] + LineEnding;
  end;
  s := s + StringList.Strings[i];
  Result := s;
end;

end.

