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
  MessageCantaraNeedsPortalUse = 'You are using Cantara as a Flatpak. ' +
    'Due to an update of the permissions, Cantara can''t access your home directory anymore which helps to make your ' +
    'system securer. However, that also means that you have to select the song repository path once again, so that Cantara ' +
    'will be granted access to that directory. You also need to select the background image once again if you had one in use.'
    + LineEnding + 'Thank you for your understanding!';

implementation

function CheckPortalUsed(ASettingsHandler: ISettingsHandler): Boolean;
begin
  if (ASettingsHandler.GetRepositoryPath <> '')
     and (not ASettingsHandler.GetSettingsFile.ReadBool('Flathub', 'Portals used', false)) then
    Result := False
  else Result := True;
end;

end.

