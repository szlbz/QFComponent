unit setfontcolor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, StdCtrls,tools;

type

  { TSetFontColorFrm }

  TSetFontColorFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SetFontColorFrm: TSetFontColorFrm;

implementation

{$R *.lfm}

{ TSetFontColorFrm }

procedure TSetFontColorFrm.Button1Click(Sender: TObject);
begin
  tools.ButtonPress:=1;
  if SelTextlength=0 then
  begin
    if CheckBox1.Checked then tools.fontcolor:='[c1]';
    if CheckBox2.Checked then tools.fontcolor:='[c2]';
    if CheckBox3.Checked then tools.fontcolor:='[c3]';
    if CheckBox4.Checked then tools.fontcolor:='[c4]';
    if CheckBox5.Checked then tools.fontcolor:='[c5]';
  end
  else
  begin
    if CheckBox1.Checked then tools.fontcolor:='<c1>';
    if CheckBox2.Checked then tools.fontcolor:='<c2>';
    if CheckBox3.Checked then tools.fontcolor:='<c3>';
    if CheckBox4.Checked then tools.fontcolor:='<c4>';
    if CheckBox5.Checked then tools.fontcolor:='<c5>';
  end;
  close;
end;

procedure TSetFontColorFrm.Button2Click(Sender: TObject);
begin
  tools.ButtonPress:=0;
  tools.fontcolor:='';
  close;
end;

procedure TSetFontColorFrm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    CheckBox2.Checked := False;
    CheckBox3.Checked := False;
    CheckBox4.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontColorFrm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox3.Checked := False;
    CheckBox4.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontColorFrm.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox2.Checked := False;
    CheckBox4.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontColorFrm.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox2.Checked := False;
    CheckBox3.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontColorFrm.CheckBox5Click(Sender: TObject);
begin
  if CheckBox5.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox2.Checked := False;
    CheckBox3.Checked := False;
    CheckBox4.Checked := False;
  end;
end;

procedure TSetFontColorFrm.FormCreate(Sender: TObject);
begin
  tools.ButtonPress:=0;
  tools.fontsize:='';
end;

end.

