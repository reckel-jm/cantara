unit PresentationCanvas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, Graphics, IntfGraphics, Math,
  fpImage;

type
  TPresentationStyleSettings = record
    BackgroundColor: TColor;
    Font: TFont;
    TextColor: TColor;
    ShowBackgroundImage: Boolean;
    BackgroundImageFilePath: String;
    Transparency: Integer;
  end;

  TPresentationCanvasHandler = class
    public
      SlideSettings: TSlideSettings;
      PresentationStyleSettings: TPresentationStyleSettings;
      Width, Height: Integer;
      constructor Create; overload;
      constructor Create(aPresentationStyleSettings: TPresentationStyleSettings; aSlideSettings: TSlideSettings); overload;
      destructor Destroy; override;
      // Will be used to adjust the brightness of the background
      procedure AdjustBrightness(Offset: Integer);
      procedure LoadBackgroundBitmap;
      function PaintSlide(Slide: TSlide): TBitmap;
    private
      BackgroundBitmap: TBitmap;
  end;

const
  PADDING:integer = 5;

implementation
procedure TPresentationCanvasHandler.AdjustBrightness(Offset: Integer);
var
  SrctfImg, TemptfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  px, py: integer;
  CurColor: TFPColor;
  SourceBitmap, DestBitmap: TBitmap;
begin
  // Here we create the necessary structures
  SourceBitmap := self.BackgroundBitmap;
  DestBitmap := TBitmap.Create;
  DestBitmap.Width:=SourceBitmap.Width;
  DestBitmap.Height:=SourceBitmap.Height;
  // And at this point we transform the color change
  Offset:=Offset * $FF;
  SrctfImg := TLazIntfImage.Create(0, 0);
  SrctfImg.LoadFromBitmap(SourceBitmap.Handle, SourceBitmap.MaskHandle);
  TemptfImg := TLazIntfImage.Create(0, 0);
  TemptfImg.DataDescription := GetDescriptionFromDevice(0);
  TemptfImg.SetSize(SrctfImg.Width,SrctfImg.Height);
  for py := 0 to SrctfImg.Height - 1 do
  begin
    for px := 0 to SrctfImg.Width - 1 do
    begin
      CurColor := SrctfImg.Colors[px, py];
      CurColor.red := EnsureRange(CurColor.red + Offset,0,$FFFF);
      CurColor.green := EnsureRange(CurColor.green + Offset,0,$FFFF);
      CurColor.blue := EnsureRange(CurColor.blue + Offset,0,$FFFF);
      TemptfImg.Colors[px, py] := CurColor;
    end;
  end;
  TemptfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  DestBitmap.Handle := ImgHandle;
  DestBitmap.MaskHandle := ImgMaskHandle;
  SrctfImg.Free;
  TemptfImg.Free;
  SourceBitmap.Destroy;
  BackgroundBitmap := DestBitmap;
end;

constructor TPresentationCanvasHandler.Create; overload;
begin
  inherited;
end;

constructor TPresentationCanvasHandler.Create(aPresentationStyleSettings: TPresentationStyleSettings; aSlideSettings: TSlideSettings); overload;
begin
  inherited Create;
  PresentationStyleSettings := aPresentationStyleSettings;
  SlideSettings := aSlideSettings;
  LoadBackgroundBitmap;
end;

destructor TPresentationCanvasHandler.Destroy;
begin
  if Assigned(BackgroundBitmap) then BackgroundBitmap.Destroy;
  inherited;
end;

procedure TPresentationCanvasHandler.LoadBackgroundBitmap;
begin
  if Assigned(BackgroundBitmap) then FreeAndNil(BackgroundBitmap);
  if PresentationStyleSettings.ShowBackgroundImage then
  begin
    BackgroundBitmap := TBitmap.Create;
    BackgroundBitmap.LoadFromFile(PresentationStyleSettings.BackgroundImageFilePath);
    AdjustBrightness(PresentationStyleSettings.Transparency);
  end;
end;

function TPresentationCanvasHandler.PaintSlide(Slide: TSlide): TBitmap;
var Bitmap: TBitmap;
  BackgroundRect, ContentRect: TRect;
begin
  Bitmap := TBitmap.Create;
  Bitmap.SetSize(self.Width, self.Height);
  with Bitmap.Canvas do
  begin
    Brush.Color := PresentationStyleSettings.BackgroundColor;
    Brush.Style := bsSolid;
    with BackgroundRect do
    begin
      Left := 0;
      Top := 0;
      Width := self.Width-1;
      Height := self.Height-1;
    end;
    FillRect(Bitmap.Canvas.ClipRect);
    FillRect(BackgroundRect);
    with TextStyle do
    begin
      Alignment:= taCenter;
      Layout:= tlCenter;
      SingleLine := False;
      WordBreak := True;
      Opaque := False;
    end;
    Font := PresentationStyleSettings.Font;
    Font.Color:=PresentationStyleSettings.TextColor;
    with ContentRect do
    begin
      Left := PADDING;
      Top := PADDING;
      Width := self.Width-Padding;
      Height := self.Height-Padding;
    end;
    TextRect(ContentRect, 0, 0, Slide.PartContent.MainText);
  end;
  Result := Bitmap;
end;

end.

