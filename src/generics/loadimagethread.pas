unit loadimagethread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PresentationCanvas, Slides, Graphics, Forms;

type

  { TLoadImageThread }

  TLoadImageThread = class(TThread)
  public
    Constructor Create(CreateSuspended : boolean;
      PresentationStyleSettings: TPresentationStyleSettings;
      SlideSettings: TSlideSettings;
      PresentationCanvas: TPresentationCanvasHandler;
      Screen: TScreen;
      ChangeBackground: Boolean;
      Slide: TSlide;
      TargetPicture: TPicture
      );
  private
    PresentationStyleSettings: TPresentationStyleSettings;
    SlideSettings: TSlideSettings;
    TargetPicture: TPicture;
    PresentationCanvas: TPresentationCanvasHandler;
    Screen: TScreen;
    ChangeBackground: Boolean;
    procedure LoadImage;
  protected
    procedure Execute; override;

  end;

implementation

{ TLoadImageThread }

constructor TLoadImageThread.Create(CreateSuspended: boolean;
  PresentationStyleSettings: TPresentationStyleSettings;
  SlideSettings: TSlideSettings;
  PresentationCanvas: TPresentationCanvasHandler; Screen: TScreen;
  ChangeBackground: Boolean; Slide: TSlide; TargetPicture: TPicture);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
  self.PresentationStyleSettings:=PresentationStyleSettings;
  self.SlideSettings:=SlideSettings;
  self.PresentationCanvas:=PresentationCanvas;
  self.Screen:=Screen;
  self.ChangeBackground:=ChangeBackground;
  self.Slide := Slide;
  self.TargetPicture:=TargetPicture;
end;

procedure TLoadImageThread.LoadImage;
begin
  TargetPicture.Assign(PresentationCanvas.PaintSlide(Slide));
end;

procedure TLoadImageThread.Execute;
begin
  PresentationCanvas.SlideSettings := self.SlideSettings;
  PresentationCanvas.PresentationStyleSettings := self.PresentationStyleSettings;
  PresentationCanvas.Width:=self.Screen.Width;
  PresentationCanvas.Height:=self.Screen.Height;
  if ChangeBackground then
    PresentationCanvas.LoadBackgroundBitmap;
  PresentationCanvas.ResizeBackgroundBitmap;
  Synchronize(@LoadImage);
end;

end.

