unit PresentationCanvas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, LCLIntf, Graphics, IntfGraphics, Math,
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
      Bitmap: TBitmap;
      function CalculateTextHeight(Font: TFont; RectWidth: Integer; TextString: String): Integer;
  end;

const
  PADDING:integer = 15;

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
  Bitmap := TBitmap.Create;
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
  Bitmap.Destroy;
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
var
  BackgroundRect, ContentRect: TRect;
  MainTextHeight, SpoilerTextHeight, MetaTextHeight: Integer;
  NormalTextFont, SpoilerTextFont, MetaTextFont: TFont;
  SpoilerRectWidth: Integer;
begin
  Bitmap.Canvas.Clear;
  Bitmap.SetSize(self.Width, self.Height);
  // Here we setup the different fonts for calculating the text height
  NormalTextFont := TFont.Create;
  NormalTextFont.Assign(PresentationStyleSettings.Font);
  NormalTextFont.Color:=PresentationStyleSettings.TextColor;
  SpoilerTextFont := TFont.Create;
  SpoilerTextFont.Assign(NormalTextFont);
  SpoilerTextFont.Height:=NormalTextFont.Height div 2;
  MetaTextFont := TFont.Create;
  MetaTextFont.Assign(NormalTextFont);
  MetaTextFont.Height:=NormalTextFont.Height div 3;
  with BackgroundRect do
  begin
    Left := 0;
    Top := 0;
    Width := self.Width-1;
    Height := self.Height-1;
  end;
  MainTextHeight := self.CalculateTextHeight(NormalTextFont, self.Width-2*Padding, Slide.PartContent.MainText);
  if Slide.PartContent.SpoilerText <> '' then
  begin
    SpoilerTextHeight := self.CalculateTextHeight(SpoilerTextFont, self.Width-2*Padding, Slide.PartContent.SpoilerText);
    SpoilerRectWidth := Round((self.Width-2*Padding)*2/3);
  end
  else SpoilerTextHeight := 0;
  MetaTextHeight := CalculateTextHeight(MetaTextFont, SpoilerRectWidth, Slide.PartContent.MetaText);
  with Bitmap.Canvas do
  begin
    Brush.Color := PresentationStyleSettings.BackgroundColor;
    Brush.Style := bsSolid;
    FillRect(Bitmap.Canvas.ClipRect);
    FillRect(BackgroundRect);
    with TextStyle do
    begin
      Alignment:= taCenter;
      Layout:= tlTop;
      SingleLine := False;
      WordBreak := True;
      Opaque := False;
    end;
    Font.Assign(NormalTextFont);
    with ContentRect do
    begin
      Left := PADDING;
      Top := Padding+(self.Height-2*Padding-MainTextHeight-SpoilerTextHeight-40) div 2;
      Width := self.Width-Padding;
      Height := MainTextHeight;
    end;
    TextRect(ContentRect, ContentRect.Left, ContentRect.Top, Slide.PartContent.MainText);
    // We paint the spoiler if desired
    if Slide.PartContent.SpoilerText <> '' then
    begin
      Font.Assign(SpoilerTextFont);
      ContentRect.Top += MainTextHeight + 20;
      ContentRect.Height:=SpoilerTextHeight;
      TextRect(ContentRect, ContentRect.Left, ContentRect.Top, Slide.PartContent.SpoilerText);
    end;
    // We paint Meta information if desired
    if Slide.PartContent.MetaText <> '' then
    begin
      ContentRect.Top := self.Height-Padding-MetaTextHeight;
      ContentRect.Left:=Padding;
      ContentRect.Height:=MetaTextHeight;
      ContentRect.Width:=SpoilerRectWidth;
      with TextStyle do
        Alignment := taLeftJustify;
      Font.Assign(MetaTextFont);
      TextRect(ContentRect, ContentRect.Left, ContentRect.Top, Slide.PartContent.MetaText);
    end;
  end;
  //Bitmap.SaveToFile(GetTempDir() + 'foto.png');
  NormalTextFont.Destroy;
  SpoilerTextFont.Destroy;
  MetaTextFont.Destroy;
  Result := Bitmap;
end;

function TPresentationCanvasHandler.CalculateTextHeight(Font: TFont; RectWidth: Integer; TextString: String): Integer;
var
  R: TRect;
begin
  Bitmap.Canvas.Font.Assign(Font);
  R  := Rect(0, 0, RectWidth, 0);
  Result := DrawText
    (Bitmap.Canvas.Handle, PChar(TextString), Length(TextString), R, dt_CalcRect Or dt_WordBreak);
end;

end.

