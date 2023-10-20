unit ResourceHandling;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils;

function LoadResourceFileIntoStringList(ResourceName: String): TStringList;

implementation

function LoadResourceFileIntoStringList(ResourceName: String): TStringList;
var
  ResourceStream: TResourceStream;
begin
  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  Result := TStringList.Create;
  Result.LoadFromStream(ResourceStream);
  FreeAndNil(ResourceStream);
end;

end.
