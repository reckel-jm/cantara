unit SongSelectionController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ContentSelectionModel;

type

  { TSongSelectionController }

  TSongSelectionController = class(TObject)
    ContentSelectionModel: TContentSelectionModel;
    RepositoryModel: TRepositoryModel;
    Destructor destroy; override;
  end;

procedure CreateAndInjectTSongSelectionController(ATfrmSongs: TfrmSongs);

implementation

procedure CreateAndInjectTSongSelectionController(ATfrmSongs: TfrmSongs);
var Controller: TSongSelectionController;
begin
  Controller := TSongSelectionController.Create;
  Controller.ContentSelectionModel := TContentSelectionModel.Create;
  Controller.RepositoryModel := TRepositoryModel.Create;
  ATfrmSongs.Controller := Controller;
end;

{ TSongSelectionController }

destructor TSongSelectionController.destroy;
begin
  inherited destroy;
  Self.ContentSelectionModel.Destroy;
  Self.RepositoryModel.Destroy;
end;

end.

