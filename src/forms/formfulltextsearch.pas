unit FormFulltextSearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, Fulltextsearch;

type

  { TfrmWrapperFulltextSearch }

  TfrmWrapperFulltextSearch = class(TForm)
    Wrapper: TFrmFulltextsearch;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    ParentForm: TForm;
  public
    function ShowModal(caller: TForm): Integer; overload;
  end;

var
  frmWrapperFulltextSearch: TfrmWrapperFulltextSearch;

implementation

uses
  SongSelection;

  {$R *.lfm}

  { TfrmWrapperFulltextSearch }

procedure TfrmWrapperFulltextSearch.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if key = VK_Escape then self.Close;
end;

procedure TfrmWrapperFulltextSearch.FormShow(Sender: TObject);
begin
  if Wrapper.IndexList = nil then Wrapper.CreateIndex;
end;

function TfrmWrapperFulltextSearch.ShowModal(Caller: TForm): Integer;
begin
  self.ParentForm := caller;
  if Assigned(self.ParentForm) then
  begin
    SetFocusedControl(Wrapper.EditSearchTerm);
    // Good Positioning
    self.ParentForm := Caller;
    self.Width := ParentForm.Width;
    self.Height := Parentform.Height Div 2;
  end;
  Result := inherited ShowModal;
end;

end.
