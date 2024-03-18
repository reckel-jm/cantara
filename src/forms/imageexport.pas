unit imageexport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ComCtrls, Menus, Slides, PresentationCanvas,

  exporterinterfaces;

type

  { TFormImageExport }

  TFormImageExport = class(TForm, ISongExporter)
    ButtonReloadPreview: TButton;
    ButtonExportAll: TButton;
    ButtonChoose: TButton;
    ButtonExportCurrent: TButton;
    CheckBoxSync: TCheckBox;
    EditFolder: TEdit;
    EditHeight: TSpinEdit;
    GroupSettings: TGroupBox;
    ImageList: TImageList;
    LabelFolder: TLabel;
    LabelHeight: TLabel;
    LabelWidth: TLabel;
    ImageListView: TListView;
    ItemExport: TMenuItem;
    ItemRemove: TMenuItem;
    PictureDirectoryDialog: TSelectDirectoryDialog;
    EditWidth: TSpinEdit;
    ImageListViewMenu: TPopupMenu;
    ProgressBar: TProgressBar;
    SaveDialog: TSaveDialog;
    Splitter: TSplitter;
    procedure ButtonExportCurrentClick(Sender: TObject);
    procedure ButtonExportAllClick(Sender: TObject);
    procedure ButtonReloadPreviewClick(Sender: TObject);
    procedure ButtonChooseClick(Sender: TObject);
    procedure EditHeightChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ItemExportClick(Sender: TObject);
    procedure ItemRemoveClick(Sender: TObject);
  private
    Scale: Double;
    ShowFirstTime: Boolean;
    FrmSongsPresentationIsRunning: Boolean;
    procedure ReadjustScale;
  public
    PresentationCanvas: TPresentationCanvasHandler;
    SlideList: TSlideList;
    procedure LoadImages;

    procedure RunExporter(PresentationIsRunning: Boolean);
  end;

var
  FormImageExport: TFormImageExport;

resourcestring
  strFolderNotValid = 'Please select a folder in which you want to save the pictures.';

implementation

{$R *.lfm}

{ TFormImageExport }

procedure TFormImageExport.ButtonChooseClick(Sender: TObject);
begin
  if PictureDirectoryDialog.Execute then
    EditFolder.Text := PictureDirectoryDialog.FileName;
end;

procedure TFormImageExport.ButtonReloadPreviewClick(Sender: TObject);
begin
  LoadImages;
end;

procedure TFormImageExport.ButtonExportAllClick(Sender: TObject);
var
  i: Integer;
  ListItem: TListItem;
  ContainerImage: TImage;
begin
  if ImageListView.Items.Count <= 0 then Exit;
  if DirectoryExists(EditFolder.Text) then
  begin
    ContainerImage := TImage.Create(FormImageExport);
    i := 0;
    for ListItem In ImageListView.Items do
    begin
      ImageList.GetBitmap(ListItem.ImageIndex, ContainerImage.Picture.Bitmap);
      ContainerImage.Picture.SaveToFile(EditFolder.Text + PathDelim +
        ListItem.Caption + '.png', '.png');
      Inc(i);
      ProgressBar.Position := Round((i + 1) / ImageListView.Items.Count * 100);
      Application.ProcessMessages;
      FormImageExport.Invalidate;
    end;
    ContainerImage.Destroy;
  end
  else
  begin
    ShowMessage(strFolderNotValid);
    ButtonChooseClick(FormImageExport);
  end;
end;

procedure TFormImageExport.ButtonExportCurrentClick(Sender: TObject);
begin
  ItemExportClick(ButtonExportCurrent);
end;

procedure TFormImageExport.EditHeightChange(Sender: TObject);
begin
  if CheckBoxSync.Checked then
    EditWidth.Value := Round(EditHeight.Value * Scale)
  else
  begin
    ReadjustScale;
  end;
end;

procedure TFormImageExport.EditWidthChange(Sender: TObject);
begin
  if CheckBoxSync.Checked then
  begin
    if Scale = 0 then Scale := 16 / 9;
    EditHeight.Value := Round(EditWidth.Value / Scale);
  end
  else
  begin
    ReadjustScale;
  end;
end;

procedure TFormImageExport.FormCreate(Sender: TObject);
begin
  SlideList := TSlideList.Create();
  PresentationCanvas := TPresentationCanvasHandler.Create();

  // load home directory into file/folder dialogs
  PictureDirectoryDialog.InitialDir := GetUserDir;
  SaveDialog.InitialDir := GetUserDir;

  ShowFirstTime := True;
end;

procedure TFormImageExport.FormDestroy(Sender: TObject);
begin
  if not Self.FrmSongsPresentationIsRunning then
  begin
    SlideList.Destroy;
    PresentationCanvas.Destroy;
  end;
end;

procedure TFormImageExport.FormShow(Sender: TObject);
begin
  if ShowFirstTime then
  begin
    Screen.UpdateMonitors;
    EditWidth.Value := Screen.Monitors[Screen.MonitorCount - 1].Width;
    EditHeight.Value := Screen.Monitors[Screen.MonitorCount - 1].Height;
    if EditHeight.Value <> 0 then Scale := EditWidth.Value / EditHeight.Value
    else
      ReadjustScale;
    ShowFirstTime := False;
  end;
end;

procedure TFormImageExport.ItemExportClick(Sender: TObject);
var
  Picture: TPicture;
  ListItem: TListItem;
begin
  if ImageListView.Items.Count <= 0 then Exit;
  if ImageListView.ItemIndex < 0 then ImageListView.ItemIndex := 0;
  if SaveDialog.Execute = False then Exit;
  Picture := TPicture.Create;
  ListItem := ImageListView.Items[ImageListView.ItemIndex];
  ImageList.GetBitmap(ListItem.ImageIndex, Picture.Bitmap);
  if LowerCase(ExtractFileExt(SaveDialog.FileName)) <> '.png' then
    SaveDialog.FileName := SaveDialog.FileName + '.png';
  Picture.SaveToFile(SaveDialog.FileName, '.png');
  Picture.Destroy;
end;

procedure TFormImageExport.ItemRemoveClick(Sender: TObject);
begin
  if ImageListView.Items.Count <= 0 then Exit;
  if ImageListView.ItemIndex < 0 then ImageListView.ItemIndex := 0;
  ImageListView.Items.Delete(ImageListView.ItemIndex);
end;

procedure TFormImageExport.RunExporter(PresentationIsRunning: Boolean);
begin
  Self.ShowOnTop;
  Application.ProcessMessages;
  Self.Invalidate;
  Self.Repaint;
  Self.LoadImages;

  Self.FrmSongsPresentationIsRunning:=PresentationIsRunning;
end;

procedure TFormImageExport.ReadjustScale;
begin
  if EditWidth.Value = 0 then EditWidth.Value := 1600;
  if EditHeight.Value = 0 then EditHeight.Value := 900;
  Scale := EditWidth.Value / EditHeight.Value;
  if Scale = 0 then
  begin
    Scale := 1600 / 900;
    EditWidth.Value := 1600;
    EditHeight.Value := 900;
  end;
end;

procedure TFormImageExport.LoadImages;
var
  i: Integer;
  Bitmap: TBitmap;
  LI: TListItem;
begin
  ImageList.Clear;
  ImageListView.Clear;
  FormImageExport.Invalidate;
  ProgressBar.Enabled := True;
  ProgressBar.Position := 0;
  PresentationCanvas.Width := EditWidth.Value;
  PresentationCanvas.Height := EditHeight.Value;
  PresentationCanvas.LoadBackgroundBitmap;
  PresentationCanvas.AdjustTransparency;
  ImageList.Width := PresentationCanvas.Width;
  ImageList.Height := PresentationCanvas.Height;
  for i := 0 to SlideList.Count - 1 do
  begin
    //PresentationCanvas.PaintSlide(SlideLIst.Items[i]).InvalidateBitmap;
    Bitmap := PresentationCanvas.PaintSlide(SlideLIst.Items[i]).Bitmap;
    ImageList.AddMasked(Bitmap, clNone);
    LI := ImageListView.Items.Add;
    LI.Caption := 'Image ' + FormatFloat('000', (i + 1));
    LI.ImageIndex := i;
    ProgressBar.Position := Round((i + 1) / SlideList.Count * 100);
    Application.ProcessMessages;
    FormImageExport.Invalidate;
  end;
end;

end.
