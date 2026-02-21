unit PresentationModels;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  {$IFNDEF NOGRAPHIC}, Graphics {$ENDIF};

type

THorizontalAlignEnum = (Align_Left, Align_Center, Align_Right);

TPadding = record
  Left: Integer;
  Right: Integer;
  Top: Integer;
  Bottom: Integer;
end;

TPresentationStyleSettings = record
  {$IFNDEF NOGRAPHIC}
  BackgroundColor: TColor;
  Font: TFont;
  TextColor: TColor;
  VerticalAlign: TTextLayout;
  {$ENDIF}
  ShowBackgroundImage: Boolean;
  BackgroundImageFilePath: String;
  Transparency: Integer;
  HorizontalAlign: THorizontalAlignEnum;
  Padding: TPadding;
  BlackScreenOnEmptySlide: Boolean;
  FadeTransition: Boolean;
  FadeDurationMs: Integer;
end;

{ Frees heap objects owned by a TPresentationStyleSettings (specifically the TFont). }
procedure DestroyPresentationStyleSettings(
  var APresentationStyleSetting: TPresentationStyleSettings);

implementation

procedure DestroyPresentationStyleSettings(
  var APresentationStyleSetting: TPresentationStyleSettings);
begin
  {$IFNDEF NOGRAPHIC}
  {$ENDIF}
end;

end.

