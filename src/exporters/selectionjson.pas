{ Unit for exporting and importing Cantara song selections in JSON format.
  The format bundles song file content, per-song style overrides, and
  background images into one self-contained portable file.

  Copyright (C) 2024 Jan Martin Reckel

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
unit SelectionJSON;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, Base64, PresentationModels,
  Graphics, LCLType, Lyrics;

type
  { Flat representation of a song's style suitable for JSON serialization.
    Contains no heap-allocated objects (no TFont), only plain value types
    and strings — records of this type need no destructor. }
  TJSONStyleData = record
    HasCustomStyle: Boolean;
    BackgroundColor: TColor;
    TextColor: TColor;
    VerticalAlign: TTextLayout;
    HorizontalAlign: THorizontalAlignEnum;
    FontName: String;
    FontSize: Integer;       // points
    FontBold: Boolean;
    FontItalic: Boolean;
    FontUnderline: Boolean;
    FontStrikeout: Boolean;
    PaddingLeft: Integer;
    PaddingRight: Integer;
    PaddingTop: Integer;
    PaddingBottom: Integer;
    ShowBackgroundImage: Boolean;
    BackgroundImageFilename: String;  // bare filename, no directory path
    Transparency: Integer;
    BlackScreenOnEmptySlide: Boolean;
    FadeTransition: Boolean;
    FadeDurationMs: Integer;
  end;

  { One song entry as stored in memory after parsing / before serialization.
    FileContent holds the raw (decoded) bytes of the song file.
    BackgroundImageData holds the raw (decoded) bytes of the background image,
    or '' if there is none. }
  TSelectionJSONSong = record
    FileName: String;            // e.g. "Amazing Grace.song"
    FileContent: String;         // raw file bytes (AnsiString)
    Style: TJSONStyleData;
    BackgroundImageData: String; // raw image bytes; '' = no image
  end;

  { Handles serialization and deserialization of a JSON song selection file. }
  TSelectionJSONFile = class
  public
    Songs: array of TSelectionJSONSong;
    constructor Create;

    { Export path: reads the song file and optional background image from disk,
      encodes them, and appends a new entry to Songs. }
    procedure AddSong(const FilePath, FileName: String;
      const HasCustomStyle: Boolean;
      const AStyle: TPresentationStyleSettings);

    { Serialise Songs to a JSON file on disk. }
    procedure SaveToFile(const AFileName: String);

    { Parse a JSON file from disk and populate Songs.
      Raises EFOpenError / EJSONParserError on failure. }
    procedure LoadFromFile(const AFileName: String);

    { Build a TPresentationStyleSettings from Songs[Idx].Style.
      BackgroundImageFilePath is intentionally left empty — the caller
      must set it after writing the background image to disk.
      Caller is responsible for calling DestroyPresentationStyleSettings
      on the returned value. }
    function SongStyleToPresSettings(Idx: Integer): TPresentationStyleSettings;

  private
    function  StyleToJSON(const S: TJSONStyleData): TJSONData;
    procedure JSONToStyleData(JData: TJSONData; out S: TJSONStyleData);
    function  FileToBase64(const AFilePath: String): String;
    function  RawToBase64(const Data: String): String;
    function  Base64ToRaw(const B64: String): String;
    function  ColorToHexStr(C: TColor): String;
    function  HexStrToColor(const S: String): TColor;
    function  ReadFileRaw(const AFilePath: String): String;
  end;

implementation

{ TSelectionJSONFile }

constructor TSelectionJSONFile.Create;
begin
  inherited Create;
  SetLength(Songs, 0);
end;

procedure TSelectionJSONFile.AddSong(const FilePath, FileName: String;
  const HasCustomStyle: Boolean; const AStyle: TPresentationStyleSettings);
var
  Entry: TSelectionJSONSong;
  S: TJSONStyleData;
begin
  Entry.FileName    := FileName;
  Entry.FileContent := ReadFileRaw(FilePath);
  Entry.BackgroundImageData := '';

  S := Default(TJSONStyleData);
  if HasCustomStyle then
  begin
    S.HasCustomStyle   := True;
    S.BackgroundColor  := AStyle.BackgroundColor;
    S.TextColor        := AStyle.TextColor;
    S.VerticalAlign    := AStyle.VerticalAlign;
    S.HorizontalAlign  := AStyle.HorizontalAlign;
    if Assigned(AStyle.Font) then
    begin
      S.FontName      := AStyle.Font.Name;
      S.FontSize      := Abs(AStyle.Font.Size);
      S.FontBold      := fsBold      in AStyle.Font.Style;
      S.FontItalic    := fsItalic    in AStyle.Font.Style;
      S.FontUnderline := fsUnderline in AStyle.Font.Style;
      S.FontStrikeout := fsStrikeOut in AStyle.Font.Style;
    end;
    S.PaddingLeft    := AStyle.Padding.Left;
    S.PaddingRight   := AStyle.Padding.Right;
    S.PaddingTop     := AStyle.Padding.Top;
    S.PaddingBottom  := AStyle.Padding.Bottom;
    S.ShowBackgroundImage   := AStyle.ShowBackgroundImage;
    S.Transparency          := AStyle.Transparency;
    S.BlackScreenOnEmptySlide := AStyle.BlackScreenOnEmptySlide;
    S.FadeTransition        := AStyle.FadeTransition;
    S.FadeDurationMs        := AStyle.FadeDurationMs;
    if AStyle.ShowBackgroundImage and
       FileExists(AStyle.BackgroundImageFilePath) then
    begin
      S.BackgroundImageFilename := ExtractFileName(AStyle.BackgroundImageFilePath);
      Entry.BackgroundImageData := ReadFileRaw(AStyle.BackgroundImageFilePath);
    end;
  end;
  Entry.Style := S;

  SetLength(Songs, Length(Songs) + 1);
  Songs[High(Songs)] := Entry;
end;

procedure TSelectionJSONFile.SaveToFile(const AFileName: String);
var
  Root: TJSONObject;
  SongsArr: TJSONArray;
  SongObj: TJSONObject;
  i: Integer;
  SL: TStringList;
begin
  Root := TJSONObject.Create;
  try
    Root.Add('version', TJSONIntegerNumber.Create(1));
    SongsArr := TJSONArray.Create;
    Root.Add('songs', SongsArr);

    for i := 0 to High(Songs) do
    begin
      SongObj := TJSONObject.Create;
      SongsArr.Add(SongObj);

      SongObj.Add('file_name',    Songs[i].FileName);
      SongObj.Add('file_content', RawToBase64(Songs[i].FileContent));
      SongObj.Add('style_setting', StyleToJSON(Songs[i].Style));

      if Songs[i].BackgroundImageData <> '' then
        SongObj.Add('background_image',
          RawToBase64(Songs[i].BackgroundImageData))
      else
        SongObj.Add('background_image', TJSONNull.Create);
    end;

    SL := TStringList.Create;
    try
      SL.Text := Root.FormatJSON;
      SL.SaveToFile(AFileName);
    finally
      SL.Free;
    end;
  finally
    Root.Free;
  end;
end;

procedure TSelectionJSONFile.LoadFromFile(const AFileName: String);
var
  SL: TStringList;
  Root: TJSONData;
  SongsArr: TJSONArray;
  SongObj: TJSONObject;
  i: Integer;
  Entry: TSelectionJSONSong;
  StyleNode, BgNode: TJSONData;
begin
  SetLength(Songs, 0);

  SL := TStringList.Create;
  SL.LoadFromFile(AFileName);
  Root := GetJSON(SL.Text);
  SL.Free;

  if Root = nil then Exit;
  try
    if Root.JSONType <> jtObject then Exit;
    SongsArr := TJSONArray(TJSONObject(Root).Find('songs', jtArray));
    if SongsArr = nil then Exit;

    for i := 0 to SongsArr.Count - 1 do
    begin
      if SongsArr.Items[i].JSONType <> jtObject then Continue;
      SongObj := TJSONObject(SongsArr.Items[i]);

      Entry := Default(TSelectionJSONSong);
      Entry.FileName    := SongObj.Get('file_name', '');
      Entry.FileContent := Base64ToRaw(SongObj.Get('file_content', ''));

      StyleNode := SongObj.Find('style_setting');
      if Assigned(StyleNode) then
        JSONToStyleData(StyleNode, Entry.Style);

      BgNode := SongObj.Find('background_image');
      if Assigned(BgNode) and (BgNode.JSONType = jtString) then
        Entry.BackgroundImageData := Base64ToRaw(BgNode.AsString)
      else
        Entry.BackgroundImageData := '';

      SetLength(Songs, Length(Songs) + 1);
      Songs[High(Songs)] := Entry;
    end;
  finally
    Root.Free;
  end;
end;

function TSelectionJSONFile.SongStyleToPresSettings(
  Idx: Integer): TPresentationStyleSettings;
var
  S: TJSONStyleData;
  FontStyle: TFontStyles;
begin
  FillChar(Result, SizeOf(Result), 0);
  S := Songs[Idx].Style;
  Result.BackgroundColor := S.BackgroundColor;
  Result.TextColor       := S.TextColor;
  Result.VerticalAlign   := S.VerticalAlign;
  Result.HorizontalAlign := S.HorizontalAlign;
  Result.Font := TFont.Create;
  Result.Font.Name := S.FontName;
  Result.Font.Size := S.FontSize;
  FontStyle := [];
  if S.FontBold      then Include(FontStyle, fsBold);
  if S.FontItalic    then Include(FontStyle, fsItalic);
  if S.FontUnderline then Include(FontStyle, fsUnderline);
  if S.FontStrikeout then Include(FontStyle, fsStrikeOut);
  Result.Font.Style := FontStyle;
  Result.Padding.Left   := S.PaddingLeft;
  Result.Padding.Right  := S.PaddingRight;
  Result.Padding.Top    := S.PaddingTop;
  Result.Padding.Bottom := S.PaddingBottom;
  Result.ShowBackgroundImage    := S.ShowBackgroundImage;
  Result.BackgroundImageFilePath := '';  // caller sets this after writing image
  Result.Transparency           := S.Transparency;
  Result.BlackScreenOnEmptySlide := S.BlackScreenOnEmptySlide;
  Result.FadeTransition  := S.FadeTransition;
  Result.FadeDurationMs  := S.FadeDurationMs;
end;

{ --- Private helpers --- }

function TSelectionJSONFile.StyleToJSON(const S: TJSONStyleData): TJSONData;
var
  Obj: TJSONObject;
begin
  if not S.HasCustomStyle then
  begin
    Result := TJSONString.Create('default');
    Exit;
  end;
  Obj := TJSONObject.Create;
  Obj.Add('background_color', ColorToHexStr(S.BackgroundColor));
  Obj.Add('text_color',       ColorToHexStr(S.TextColor));
  Obj.Add('show_background_image',   S.ShowBackgroundImage);
  Obj.Add('background_image_filename', S.BackgroundImageFilename);
  Obj.Add('transparency', S.Transparency);
  case S.HorizontalAlign of
    Align_Left:   Obj.Add('horizontal_align', 'left');
    Align_Right:  Obj.Add('horizontal_align', 'right');
    else          Obj.Add('horizontal_align', 'center');
  end;
  case S.VerticalAlign of
    tlTop:    Obj.Add('vertical_align', 'top');
    tlBottom: Obj.Add('vertical_align', 'bottom');
    else      Obj.Add('vertical_align', 'middle');
  end;
  Obj.Add('font_name',      S.FontName);
  Obj.Add('font_size',      S.FontSize);
  Obj.Add('font_bold',      S.FontBold);
  Obj.Add('font_italic',    S.FontItalic);
  Obj.Add('font_underline', S.FontUnderline);
  Obj.Add('font_strikeout', S.FontStrikeout);
  Obj.Add('padding_left',   S.PaddingLeft);
  Obj.Add('padding_right',  S.PaddingRight);
  Obj.Add('padding_top',    S.PaddingTop);
  Obj.Add('padding_bottom', S.PaddingBottom);
  Obj.Add('black_screen_on_empty_slide', S.BlackScreenOnEmptySlide);
  Obj.Add('fade_transition',   S.FadeTransition);
  Obj.Add('fade_duration_ms',  S.FadeDurationMs);
  Result := Obj;
end;

procedure TSelectionJSONFile.JSONToStyleData(JData: TJSONData;
  out S: TJSONStyleData);
var
  Obj: TJSONObject;
  HAlign, VAlign: String;
begin
  S := Default(TJSONStyleData);
  if JData.JSONType = jtString then
  begin
    // "default" — no custom style
    S.HasCustomStyle := False;
    Exit;
  end;
  if JData.JSONType <> jtObject then Exit;
  Obj := TJSONObject(JData);
  S.HasCustomStyle  := True;
  S.BackgroundColor := HexStrToColor(Obj.Get('background_color', '000000'));
  S.TextColor       := HexStrToColor(Obj.Get('text_color', 'FFFFFF'));
  S.ShowBackgroundImage    := Obj.Get('show_background_image', False);
  S.BackgroundImageFilename := Obj.Get('background_image_filename', '');
  S.Transparency    := Obj.Get('transparency', 0);
  HAlign := Obj.Get('horizontal_align', 'center');
  if HAlign = 'left'  then S.HorizontalAlign := Align_Left
  else if HAlign = 'right' then S.HorizontalAlign := Align_Right
  else S.HorizontalAlign := Align_Center;
  VAlign := Obj.Get('vertical_align', 'middle');
  if VAlign = 'top'    then S.VerticalAlign := tlTop
  else if VAlign = 'bottom' then S.VerticalAlign := tlBottom
  else S.VerticalAlign := tlCenter;
  S.FontName      := Obj.Get('font_name',   'Arial');
  S.FontSize      := Obj.Get('font_size',   32);
  S.FontBold      := Obj.Get('font_bold',      False);
  S.FontItalic    := Obj.Get('font_italic',    False);
  S.FontUnderline := Obj.Get('font_underline', False);
  S.FontStrikeout := Obj.Get('font_strikeout', False);
  S.PaddingLeft   := Obj.Get('padding_left',   0);
  S.PaddingRight  := Obj.Get('padding_right',  0);
  S.PaddingTop    := Obj.Get('padding_top',    0);
  S.PaddingBottom := Obj.Get('padding_bottom', 0);
  S.BlackScreenOnEmptySlide := Obj.Get('black_screen_on_empty_slide', False);
  S.FadeTransition  := Obj.Get('fade_transition',  False);
  S.FadeDurationMs  := Obj.Get('fade_duration_ms', 500);
end;

function TSelectionJSONFile.ReadFileRaw(const AFilePath: String): String;
var
  FS: TFileStream;
begin
  Result := '';
  FS := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size = 0 then Exit;
    SetLength(Result, FS.Size);
    FS.Read(Result[1], FS.Size);
  finally
    FS.Free;
  end;
end;

function TSelectionJSONFile.FileToBase64(const AFilePath: String): String;
var
  InputStream:  TFileStream;
  OutputStream: TStringStream;
  Encoder:      TBase64EncodingStream;
begin
  Result := '';
  InputStream  := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  OutputStream := TStringStream.Create('');
  Encoder      := TBase64EncodingStream.Create(OutputStream);
  try
    Encoder.CopyFrom(InputStream, InputStream.Size);
    Encoder.Flush;
    Result := OutputStream.DataString;
  finally
    Encoder.Free;
    OutputStream.Free;
    InputStream.Free;
  end;
end;

function TSelectionJSONFile.RawToBase64(const Data: String): String;
var
  InputStream:  TStringStream;
  OutputStream: TStringStream;
  Encoder:      TBase64EncodingStream;
begin
  Result := '';
  if Data = '' then Exit;
  InputStream  := TStringStream.Create(Data);
  OutputStream := TStringStream.Create('');
  Encoder      := TBase64EncodingStream.Create(OutputStream);
  try
    Encoder.CopyFrom(InputStream, InputStream.Size);
    Encoder.Flush;
    Result := OutputStream.DataString;
  finally
    Encoder.Free;
    OutputStream.Free;
    InputStream.Free;
  end;
end;

function TSelectionJSONFile.Base64ToRaw(const B64: String): String;
var
  InputStream:  TStringStream;
  Decoder:      TBase64DecodingStream;
  OutputStream: TMemoryStream;
begin
  Result := '';
  if B64 = '' then Exit;
  InputStream  := TStringStream.Create(B64);
  Decoder      := TBase64DecodingStream.Create(InputStream, bdmMIME);
  OutputStream := TMemoryStream.Create;
  try
    OutputStream.CopyFrom(Decoder, 0);
    if OutputStream.Size > 0 then
    begin
      SetLength(Result, OutputStream.Size);
      OutputStream.Position := 0;
      OutputStream.Read(Result[1], OutputStream.Size);
    end;
  finally
    OutputStream.Free;
    Decoder.Free;
    InputStream.Free;
  end;
end;

function TSelectionJSONFile.ColorToHexStr(C: TColor): String;
var
  R, G, B: Byte;
begin
  RedGreenBlue(C, R, G, B);
  Result := Format('%.2x%.2x%.2x', [R, G, B]);
end;

function TSelectionJSONFile.HexStrToColor(const S: String): TColor;
var
  R, G, B: Integer;
begin
  if Length(S) < 6 then begin Result := clBlack; Exit; end;
  R := StrToIntDef('$' + Copy(S, 1, 2), 0);
  G := StrToIntDef('$' + Copy(S, 3, 2), 0);
  B := StrToIntDef('$' + Copy(S, 5, 2), 0);
  Result := RGBToColor(R, G, B);
end;

end.
