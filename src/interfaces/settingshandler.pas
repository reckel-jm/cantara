unit settingshandler;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, PresentationModels, IniFiles;

type
  { An interface used for handling the user settings. It is implemented by the settings form }
  ISettingsHandler = interface
    function GetRepositoryPath: String;
    function ExportSlideSettings: TSlideSettings;
    function ExportPresentationStyleSettings: TPresentationStyleSettings;
    function GetSettingsFile: TINIFile;
    procedure SetPortalsUsedFlag;
  end;

implementation

end.

