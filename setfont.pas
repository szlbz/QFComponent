unit setfont;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, StdCtrls,tools;

type

  { TSetFontFrm }

  TSetFontFrm = class(TForm)
    FontDialog1: TFontDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  SetFontFrm: TSetFontFrm;

implementation

{$R *.lfm}

{ TSetFontFrm }

procedure TSetFontFrm.FormCreate(Sender: TObject);
begin
  tools.ButtonPress:=0;
  tools.fontname:='';
  if FontDialog1.Execute then
   tools.FontName:= FontDialog1.Font.Name;
end;

procedure TSetFontFrm.FormShow(Sender: TObject);
begin
  tools.ButtonPress:=1;
  close;
end;

end.

