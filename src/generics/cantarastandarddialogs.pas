{
  This unit provides helper functions for the handling of StandardDialogs such as
  OpenDialog or SaveDialog.
  Note that the functions and procedures of that unit will not create or destroy
  such Dialogs. They should live in their parent form and be given as an
  argument to the individual function.
}
unit CantaraStandardDialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

{
  Handles the execution of a FileDialog (in most cases OpenDialog or SaveDialog).
  It takes one FileDialog as argument and
  automatically sets the initial directory to the user directory and returns
  the result of the execution of the dialog (Boolean).
}
function ExecuteCantaraFileDialog(AFileDialog: TFileDialog): Boolean;

implementation

function ExecuteCantaraFileDialog(AFileDialog: TFileDialog): Boolean;
begin
  AFileDialog.InitialDir := GetUserDir();
  Result := AFileDialog.Execute;
end;


end.

