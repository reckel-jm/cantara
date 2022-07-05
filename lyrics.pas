unit lyrics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings;
type
TSongPart = record
      Identifier: String;
      Content: String;
  end;

  //TSong = Class ( TObject )
    //function Get(index: Integer): TSongPart;
    //procedure Put(index: Integer): TSongPart;
    //property Items(index: Integer): TKontakt read get write put; default;
    //property title: String;
    //property author: String;
    //property composer: String;
    //property copyright: string;
    //property cclinumber: string;
  //end;

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
  Result := s;
end;

end.

