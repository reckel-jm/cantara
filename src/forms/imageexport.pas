unit imageexport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ComCtrls, Slides, PresentationCanvas;

type

  { TFormImageExport }

  TFormImageExport = class(TForm)
    ButtonReloadPreview: TButton;
    ButtonExportAll: TButton;
    ButtonChoose: TButton;
    ButtonExportAll1: TButton;
    CheckBoxSync: TCheckBox;
    EditFolder: TEdit;
    EditHeight: TSpinEdit;
    GroupSettings: TGroupBox;
    ImageList: TImageList;
    LabelFolder: TLabel;
    LabelHeight: TLabel;
    LabelWidth: TLabel;
    ImageListView: TListView;
    PictureDirectoryDialog: TSelectDirectoryDialog;
    EditWidth: TSpinEdit;
    ProgressBar: TProgressBar;
    procedure ButtonReloadPreviewClick(Sender: TObject);
    procedure ButtonChooseClick(Sender: TObject);
    procedure EditHeightChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupSettingsClick(Sender: TObject);
  private
    Scale: Double;
    procedure ReadjustScale;
  public
    PresentationCanvas: TPresentationCanvasHandler;
    SlideList: TSlideList;
    procedure LoadImages;
  end;

var
  FormImageExport: TFormImageExport;

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

procedure TFormImageExport.EditHeightChange(Sender: TObject);
begin
  if CheckBoxSync.Checked then
     EditWidth.Value := Round(EditHeight.Value*Scale)
  else
  begin
    ReadjustScale;
  end;
end;

procedure TFormImageExport.EditWidthChange(Sender: TObject);
begin
  if CheckBoxSync.Checked then
     EditHeight.Value := Round(EditWidth.Value/Scale)
  else
  begin
    ReadjustScale;
  end;
end;

procedure TFormImageExport.FormCreate(Sender: TObject);
begin
  EditWidth.Value := Screen.Monitors[Screen.MonitorCount-1].Width;
  EditHeight.Value := Screen.Monitors[Screen.MonitorCount-1].Height;
  ReadjustScale;
  SlideList := TSlideList.Create();
  PresentationCanvas := TPresentationCanvasHandler.Create();
end;

procedure TFormImageExport.FormDestroy(Sender: TObject);
begin
  SlideList.Destroy;
  PresentationCanvas.Destroy;
end;

procedure TFormImageExport.FormShow(Sender: TObject);
begin

end;

procedure TFormImageExport.GroupSettingsClick(Sender: TObject);
begin

end;

procedure TFormImageExport.ReadjustScale;
begin
  if EditWidth.Value = 0 then EditWidth.Value := 800;
  if EditHeight.Value = 0 then EditHeight.Value := 600;
  Scale := EditWidth.Value / EditHeight.Value;
  if Scale = 0 then
  begin
    Scale := 800/600;
    EditWidth.Value := 800;
    EditHeight.Value := 600;
  end;
end;

procedure TFormImageExport.LoadImages;
var i: Integer;
  Bitmap: TBitmap;
  LI: TListItem;
begin
  ImageList.Clear;
  ImageListView.Clear;
  FormImageExport.Invalidate;
  ProgressBar.Enabled:=True;
  ProgressBar.Position:=0;
  PresentationCanvas.Width:=EditWidth.Value;
  PresentationCanvas.Height:=EditHeight.Value;
  PresentationCanvas.LoadBackgroundBitmap;
  PresentationCanvas.AdjustBrightness;
  ImageList.Width:=PresentationCanvas.Width;
  ImageList.Height:=PresentationCanvas.Height;
  for i := 0 to SlideList.Count - 1 do
  begin
    Bitmap := PresentationCanvas.PaintSlide(SlideLIst.Items[i]);
    ImageList.AddMasked(Bitmap, clNone);
    LI := ImageListView.Items.Add;
    LI.Caption := 'Image ' + FormatFloat('000', (i+1));
    LI.ImageIndex:=i;
    ProgressBar.Position:=Round((i+1)/SlideList.Count*100);
    FormImageExport.Invalidate;
  end;
end;

end.

