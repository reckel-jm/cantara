unit settingsdetailed;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Spin, ValEdit, StdCtrls,
  PresentationCanvas;

type

  { TFrameSettingsDetailed }

  TFrameSettingsDetailed = class(TFrame)
    GroupBoxPadding: TGroupBox;
    LabelLeft: TLabel;
    LabelRight: TLabel;
    LabelTop: TLabel;
    LabalBottom: TLabel;
    LeftSpin: TSpinEdit;
    RightSpin: TSpinEdit;
    TopSpin: TSpinEdit;
    BottomSpin: TSpinEdit;
  private

  public
    procedure ImportPadding(Padding: TPadding);
    function ExportPadding: TPadding;
  end;

implementation

{$R *.lfm}

procedure TFrameSettingsDetailed.ImportPadding(Padding: TPadding);
begin
  LeftSpin.Value := Padding.Left;
  TopSpin.Value := Padding.Top;
  RightSpin.Value := Padding.Right;
  BottomSpin.Value := Padding.Bottom;
end;

function TFrameSettingsDetailed.ExportPadding: TPadding;
begin
  Result.Left := LeftSpin.Value;
  Result.Top := TopSpin.Value;
  Result.Right := RightSpin.Value;
  Result.Bottom := BottomSpin.Value;
end;

end.
