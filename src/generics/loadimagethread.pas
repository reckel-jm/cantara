unit loadimagethread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PresentationCanvas, Slides, Graphics, Forms;

type

  { TLoadImageThread }

  TLoadImageThread = class(TThread)
  public
    Constructor Create(CreateSuspended : boolean);
    procedure LoadData(PresentationStyleSettings: TPresentationStyleSettings;
      SlideSettings: TSlideSettings;
      PresentationCanvas: TPresentationCanvasHandler;
      Screen: TScreen;
      ChangeBackground: Boolean;
      Slide: TSlide;
      TargetPicture: TPicture);
  procedure RunOnce;
  private
    blocked: Boolean;
    PresentationStyleSettings: TPresentationStyleSettings;
    SlideSettings: TSlideSettings;
    TargetPicture: TPicture;
    PresentationCanvas: TPresentationCanvasHandler;
    Screen: TScreen;
    ChangeBackground: Boolean;
    Slide: TSlide;
    halted: Boolean;
    Paused: Boolean;
    procedure LoadImage;
  protected
    procedure Execute; override;
  end;

implementation

Uses Settings;

{ TLoadImageThread }

constructor TLoadImageThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);
  halted := True;
  blocked := False;
end;

procedure TLoadImageThread.LoadData(
  PresentationStyleSettings: TPresentationStyleSettings;
  SlideSettings: TSlideSettings;
  PresentationCanvas: TPresentationCanvasHandler; Screen: TScreen;
  ChangeBackground: Boolean; Slide: TSlide; TargetPicture: TPicture);
begin
  self.ChangeBackground:=ChangeBackground;
  if self.PresentationStyleSettings.Transparency <> PresentationStyleSettings.Transparency then
  begin
    blocked := False;
    self.ChangeBackground:=True;
  end;
  self.PresentationStyleSettings:=PresentationStyleSettings;
  self.SlideSettings:=SlideSettings;
  self.PresentationCanvas:=PresentationCanvas;
  self.Screen:=Screen;
  self.Slide := Slide;
  self.TargetPicture:=TargetPicture;
end;

procedure TLoadImageThread.RunOnce;
begin
  halted := False;
end;

procedure TLoadImageThread.LoadImage;
begin
  TargetPicture.Assign(PresentationCanvas.PaintSlide(Slide));
end;

procedure TLoadImageThread.Execute;
var Skip: Boolean;
begin
  if Blocked then Exit else Blocked := True;
  while (not Terminated) do
  begin
    if not halted and frmSettings.Visible then
    begin
      Skip := False;
      if not Assigned(PresentationCanvas) then Skip := True;
      if not Assigned(Screen) then Skip := True;
      if Screen.Width = 0 then Skip := True;
      if Screen.Height = 0 then Skip := True;
      if not Assigned(TargetPicture) then Skip := True;
      if not Skip then
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
      halted := True;
    end else Sleep(2);
  end;
  Blocked := False;
end;

end.

