unit PresentationCanvas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, LCLIntf, Graphics, graphtype,
  intfgraphics, Math, LazCanvas,
  StrUtils, // for SplitString
  fpImage, PresentationModels, BGRABitmap, BGRABitmapTypes;

type


  TPresentationCanvasHandler = class
  public
    SlideSettings: TSlideSettings;
    PresentationStyleSettings: TPresentationStyleSettings;
    Width, Height: Integer;
    Bitmap: TBGRABitmap;
    AdjustedBackgroundPicture: TBGRABitmap;
    ResizedBackgroundBitmap: TBGRABitmap;
    constructor Create; overload;
    constructor Create(aPresentationStyleSettings: TPresentationStyleSettings;
      aSlideSettings: TSlideSettings); overload;
    destructor Destroy; override;
    // Will be used to adjust the brightness of the background
    procedure AdjustTransparency;
    procedure LoadBackgroundBitmap;
    procedure ResizeBackgroundBitmap;
    function PaintSlide(Slide: TSlide): TBGRABitmap;
  private
    BackgroundPicture: TBGRABitmap;
    function CalculateTextHeight(Font: TFont; RectWidth: Integer;
      TextString: String): Integer;
  end;

const
  PADDING: Integer = 15;
  MINSPOILERDISTANCE: Integer = 10;
  MORELYRICSINDICATOR: String = '...';

procedure DestroyPresentationStyleSettings(var APresentationStyleSetting: TPresentationStyleSettings);

implementation

procedure DestroyPresentationStyleSettings(
  var APresentationStyleSetting: TPresentationStyleSettings);
begin
  FreeAndNil(APresentationStyleSetting.Font);
end;

procedure TPresentationCanvasHandler.AdjustTransparency;
var
  x, y: Integer;
  p: PBGRAPixel;
  TransparentBackgroundImage: TBGRABitmap;
begin

  if PresentationStyleSettings.Transparency = 0 then
  begin
    AdjustedBackgroundPicture.Assign(BackgroundPicture);
    Exit;
  end;

  AdjustedBackgroundPicture.SetSize(BackgroundPicture.Width, BackgroundPicture.Height);
  AdjustedBackgroundPicture.FillRect(0,0,
                                         AdjustedBackgroundPicture.Width-1,
                                         AdjustedBackgroundPicture.Height-1,
                                         ColorToBGRA(PresentationStyleSettings.BackgroundColor)
                                     );

  TransparentBackgroundImage := TBGRABitmap.Create;
  TransparentBackgroundImage.Assign(BackgroundPicture);

  for y := 0 to TransparentBackgroundImage.Height-1 do
  begin
    p := TransparentBackgroundImage.Scanline[y];
    for x := 0 to TransparentBackgroundImage.Width-1 do
    begin
      p^.alpha := 255-Round(Abs(PresentationStyleSettings.Transparency*2.55));
      inc(p);
    end;
  end;

  AdjustedBackgroundPicture.PutImage(0,0,TransparentBackgroundImage,dmDrawWithTransparency);

  TransparentBackgroundImage.Destroy;
end;

constructor TPresentationCanvasHandler.Create; overload;
begin
  inherited;
  Bitmap := TBGRABitmap.Create;
  BackgroundPicture := TBGRABitmap.Create;
  AdjustedBackgroundPicture := TBGRABitmap.Create;
  ResizedBackgroundBitmap := TBGRABitmap.Create;
end;

constructor TPresentationCanvasHandler.Create(aPresentationStyleSettings:
  TPresentationStyleSettings; aSlideSettings: TSlideSettings); overload;
begin
  Create;
  PresentationStyleSettings := aPresentationStyleSettings;
  SlideSettings := aSlideSettings;
  LoadBackgroundBitmap;
end;

destructor TPresentationCanvasHandler.Destroy;
begin
  Bitmap.Destroy;
  BackgroundPicture.Destroy;
  ResizedBackgroundBitmap.Destroy;
  AdjustedBackgroundPicture.Destroy;
  DestroyPresentationStyleSettings(Self.PresentationStyleSettings);
  inherited;
end;

procedure TPresentationCanvasHandler.LoadBackgroundBitmap;
begin
  if (PresentationStyleSettings.ShowBackgroundImage) then
  begin
    try
      BackgroundPicture.LoadFromFile(PresentationStyleSettings.BackgroundImageFilePath);
      AdjustTransparency;
      ResizeBackgroundBitmap;
    except
      PresentationStyleSettings.ShowBackgroundImage := False;
    end;
  end;
end;

procedure TPresentationCanvasHandler.ResizeBackgroundBitmap;
var
  DestRect: TRect;
  NewHeight, NewWidth: Integer;
begin
  // Prevent a Division by Zero Exception
  if (self.Height = 0) Or (AdjustedBackgroundPicture.Height = 0) Or
    (AdjustedBackgroundPicture.Width = 0) then Exit;

  ResizedBackgroundBitmap.Fill(clBlack);

  if self.Width / self.Height >= AdjustedBackgroundPicture.Width /
    AdjustedBackgroundPicture.Height then
  begin
    NewHeight := Ceil(self.Width * AdjustedBackgroundPicture.Height /
      AdjustedBackgroundPicture.Width);
    if AdjustedBackgroundPicture.Height > self.Height then
      DestRect.Top := Max(-(Trunc(
        (AdjustedBackgroundPicture.Height - self.Height) / 2)), 0)
    else
      DestRect.Top := 0;
    DestRect.Left := 0;
    DestRect.Height := Max(newHeight, self.Height);
    DestRect.Width := Ceil(DestRect.Height * AdjustedBackgroundPicture.Width /
      AdjustedBackgroundPicture.Height);
  end

  else
  begin
    newWidth := Ceil(self.Height * AdjustedBackgroundPicture.Width /
      AdjustedBackgroundPicture.Height);
    if AdjustedBackgroundPicture.Width > self.Width then
      DestRect.Left := Max(-(Trunc(
        (AdjustedBackgroundPicture.Width - self.Width) / 2)), 0)
    else
      DestRect.Left := 0;
    DestRect.Top := 0;
    DestRect.Width := Max(newWidth, self.Width);
    DestRect.Height := Ceil(DestRect.Width * AdjustedBackgroundPicture.Height /
      AdjustedBackgroundPicture.Width);
  end;

  ResizedBackgroundBitmap.SetSize(self.Width, self.Height);
  ResizedBackgroundBitmap.Canvas.StretchDraw(DestRect, AdjustedBackgroundPicture.Bitmap);
end;

function TPresentationCanvasHandler.PaintSlide(Slide: TSlide): TBGRABitmap;
var
  BackgroundRect, ContentRect: TRect;
  MainTextHeight, SpoilerTextHeight, MetaTextHeight: Integer;
  NormalTextFont, SpoilerTextFont, MetaTextFont: TFont;
  SpoilerRectWidth: Integer;
  SpoilerDistance: Integer;
  SpoilerText: String;
  DefaultSpoilerDistance: Integer;
begin
  Bitmap.Fill(clBlack);
  Bitmap.SetSize(self.Width, self.Height);
  // Here we setup the different fonts for calculating the text height
  NormalTextFont := TFont.Create;
  NormalTextFont.Assign(PresentationStyleSettings.Font);
  NormalTextFont.Color := PresentationStyleSettings.TextColor;
  DefaultSpoilerDistance := Round(NormalTextFont.GetTextHeight('gJ') * 3.5);
  SpoilerTextFont := TFont.Create;
  SpoilerTextFont.Assign(NormalTextFont);
  SpoilerTextFont.Height := NormalTextFont.Height Div 2;
  SpoilerRectWidth := Round((self.Width - PresentationStyleSettings.Padding.Left -
    PresentationStyleSettings.Padding.Right) * 2 / 3);
  MetaTextFont := TFont.Create;
  MetaTextFont.Assign(NormalTextFont);
  MetaTextFont.Height := NormalTextFont.Height Div 3;

  with BackgroundRect do
  begin
    Left := 0;
    Top := 0;
    Width := self.Width;
    Height := self.Height;
  end;

  MainTextHeight := self.CalculateTextHeight(NormalTextFont, self.Width -
    PresentationStyleSettings.Padding.Left - PresentationStyleSettings.Padding.Right,
    Slide.PartContent.MainText);
  MetaTextHeight := CalculateTextHeight(MetaTextFont, SpoilerRectWidth,
    Slide.PartContent.MetaText);

  SpoilerText := Slide.PartContent.SpoilerText;
  if SpoilerText <> '' then
  begin
    SpoilerTextHeight := self.CalculateTextHeight(SpoilerTextFont,
      SpoilerRectWidth, SpoilerText);
    // Check whether spoiler fits
    SpoilerDistance := Min(DEFAULTSPOILERDISTANCE, Height -
      (PresentationStyleSettings.Padding.Top + PresentationStyleSettings.Padding.Bottom +
      MainTextHeight + SpoilerTextHeight + DEFAULTSPOILERDISTANCE -
      MINSPOILERDISTANCE - MetaTextHeight));
    if SpoilerDistance < DEFAULTSPOILERDISTANCE then
    begin
      SpoilerText := SplitString(SpoilerText, LineEnding)[0] + MoreLyricsIndicator;
      // Now we calculate the height again
      SpoilerTextHeight := self.CalculateTextHeight(SpoilerTextFont,
        self.Width - PresentationStyleSettings.Padding.Left -
        PresentationStyleSettings.Padding.Right, SpoilerText);
      SpoilerDistance := (Height - (PresentationStyleSettings.Padding.Top +
        PresentationStyleSettings.Padding.Bottom + MainTextHeight +
        2 * SpoilerTextHeight - MetaTextHeight)) Div 2;
      if SpoilerDistance < MINSPOILERDISTANCE then
      begin
        SpoilerText := '';
        SpoilerTextHeight := 0;
        SpoilerDistance := 0;
      end;
    end;
  end

  else
  begin
    SpoilerTextHeight := 0;
    SpoilerDistance := 0;
  end;

  with Bitmap.Canvas do
  begin
    Brush.Color := PresentationStyleSettings.BackgroundColor;
    Brush.Style := bsSolid;
    FillRect(Bitmap.Canvas.ClipRect);
    FillRect(BackgroundRect);
    //Insert Background

    if PresentationStyleSettings.ShowBackgroundImage then
    begin
      BitBlt(Bitmap.Canvas.Handle, 0, 0, self.Width, self.Height,
        ResizedBackgroundBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    end;

    with TextStyle do
    begin
      case PresentationStyleSettings.HorizontalAlign of
        Align_Left: Alignment := taLeftJustify;
        Align_Center: Alignment := taCenter;
        Align_Right: Alignment := taRightJustify;
      end;
      Layout := tlTop;
      SingleLine := False;
      WordBreak := True;
      Opaque := False;
    end;

    Font.Assign(NormalTextFont);
    with ContentRect do
    begin
      Left := PresentationStyleSettings.Padding.Left;
      case PresentationStyleSettings.VerticalAlign of
        tlTop: Top := PresentationStyleSettings.Padding.Top;
        tlCenter: Top := PresentationStyleSettings.Padding.Top +
            (self.Height - PresentationStyleSettings.Padding.Top -
            PresentationStyleSettings.Padding.Bottom - MainTextHeight -
            SpoilerTextHeight - SpoilerDistance) Div 2;
        tlBottom: Top := self.Height - PresentationStyleSettings.Padding.Bottom -
            MainTextHeight - SpoilerTextHeight - SpoilerDistance;
      end;
      Width := self.Width - PresentationStyleSettings.Padding.Right -
        PresentationStyleSettings.Padding.Left;
      Height := MainTextHeight;
    end;

    { Make the Title bold }
    if Slide.SlideType = TitleSlide then
      Font.Bold := True;
    TextRect(ContentRect, ContentRect.Left, ContentRect.Top, Slide.PartContent.MainText);

    { Paint the spoiler if desired }
    if SpoilerText <> '' then
    begin
      Font.Assign(SpoilerTextFont);
      ContentRect.Top += MainTextHeight + SpoilerDistance;
      ContentRect.Height := SpoilerTextHeight;
      TextRect(ContentRect, ContentRect.Left, ContentRect.Top, SpoilerText);
    end;

    // We paint Meta information if desired
    if Slide.PartContent.MetaText <> '' then
    begin
      ContentRect.Top := self.Height - PresentationStyleSettings.Padding.Bottom -
        MetaTextHeight;
      ContentRect.Left := PresentationStyleSettings.Padding.Left;
      ContentRect.Height := MetaTextHeight;
      ContentRect.Width := SpoilerRectWidth;
      with TextStyle do
      begin
        Alignment := taLeftJustify;
        Layout := tlBottom;
      end;
      Font.Assign(MetaTextFont);
      TextRect(ContentRect, ContentRect.Left, ContentRect.Top,
        Slide.PartContent.MetaText);
    end;

  end;

  NormalTextFont.Destroy;
  SpoilerTextFont.Destroy;
  MetaTextFont.Destroy;
  Result := Bitmap;
end;

function TPresentationCanvasHandler.CalculateTextHeight(Font: TFont;
  RectWidth: Integer; TextString: String): Integer;
var
  R: TRect;
begin
  Bitmap.Canvas.Font.Assign(Font);
  R := Rect(0, 0, RectWidth, 0);
  Result := DrawText(Bitmap.Canvas.Handle, PChar(TextString),
    Length(TextString), R, dt_CalcRect Or dt_WordBreak);
end;

end.
