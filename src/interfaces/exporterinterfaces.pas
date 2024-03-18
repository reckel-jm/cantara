unit exporterinterfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  ISongExporter = interface
    procedure RunExporter(PresentationIsRunning: Boolean);
  end;

implementation

end.

