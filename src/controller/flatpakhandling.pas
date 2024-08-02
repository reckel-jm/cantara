unit flatpakhandling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SettingsHandler;

{ This checks that the user has used the flatpak portal at least once, so that Cantara
  can access the song repository and the pictures.
  If this is not the case, then make sure to ask the user. }
function CheckPortalUsed(ASettingsHandler: ISettingsHandler): Boolean;

ResourceString
  MessageCantaraNeedsPortalUse = 'You are using Cantara with Flathub. Cantara does now not require any default permissions and therefore is not able to access any files or folders unless you have selected them in a file chooser dialog. Please reselect the song repository once again – and also the background image if in use – so that they can be opened by Cantara.';

implementation

function CheckPortalUsed(ASettingsHandler: ISettingsHandler): Boolean;
begin
  if (ASettingsHandler.GetRepositoryPath <> '')
     and (not ASettingsHandler.GetSettingsFile.ReadBool('Flathub', 'Portals used', false)) then
    Result := False
  else Result := True;
end;

end.

