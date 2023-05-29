unit settingspadding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, settingsdetailed;

type

  { TFormPadding }

  TFormPadding = class(TForm)
    frmSettingsDetailed: TFrameSettingsDetailed;
  private

  public

  end;

var
  FormPadding: TFormPadding;

implementation

{$R *.lfm}

end.

