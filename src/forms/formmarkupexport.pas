unit FormMarkupExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit;

type

  { TFrmMarkupExport }

  TFrmMarkupExport = class(TForm)
    Button1: TButton;
    btnSaveToFile: TButton;
    Result: TGroupBox;
    ResultEdit: TSynEdit;
    Splitter1: TSplitter;
    TemplateEdit: TSynEdit;
    TemplateBox: TGroupBox;
  private

  public

  end;

var
  FrmMarkupExport: TFrmMarkupExport;

implementation

{$R *.lfm}

end.

