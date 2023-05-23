unit ResourceHandling;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils;

Function LoadResourceFileIntoStringList(ResourceName: String): TStringList;

implementation

Function LoadResourceFileIntoStringList(ResourceName: String): TStringList;
var ResourceStream: TResourceStream;
begin
  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  Result := TStringList.Create;
  Result.LoadFromStream(ResourceStream);
  FreeAndNil(ResourceStream);
end;

end.

