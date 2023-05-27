unit settingsdetailed;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Spin, ValEdit, StdCtrls;

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

  end;

implementation

{$R *.lfm}

end.

