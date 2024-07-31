unit thirdpartylibraries;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IpFileBroker, IpHtml,
  {Because we want to read the html content from a resource file }
  ResourceHandling,

  {For OpenURL}
  lclintf;

type

  { TThirdPartyLibrariesForm }

  TThirdPartyLibrariesForm = class(TForm)
    IpHtmlPanel: TIpHtmlPanel;
    procedure FormCreate(Sender: TObject);
  private
    { We want to lazy load the html only once when the user opens the window for the first time. We'll check that with the var }
    AlreadyLoaded: Boolean;
  public

  end;

var
  ThirdPartyLibrariesForm: TThirdPartyLibrariesForm;


implementation

{$R *.lfm}

{ TThirdPartyLibrariesForm }

procedure TThirdPartyLibrariesForm.FormCreate(Sender: TObject);
var
  pHTML: TIpHTML;
  ResourceFile: TStringList;
begin
  if AlreadyLoaded then Exit;

  ResourceFile := LoadResourceFileIntoStringList('THIRDPARTIES.HTML');
  pHTML := TIpHTML.Create;
  pHtml.LoadFromStream(TStringStream.Create(ResourceFile.Text));
  IpHtmlPanel.SetHtml(pHTML);
  ResourceFile.Free;
end;

end.

