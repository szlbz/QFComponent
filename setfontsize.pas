unit SetFontSize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,tools;

type

  { TSetFontSizeFrm }

  TSetFontSizeFrm = class(TForm)
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
  SetFontSizeFrm: TSetFontSizeFrm;

implementation

{$R *.lfm}

{ TSetFontSizeFrm }

procedure TSetFontSizeFrm.Button1Click(Sender: TObject);
begin
  tools.ButtonPress:=1;
  if CheckBox1.Checked then tools.fontsize:='[s1]';
  if CheckBox2.Checked then tools.fontsize:='[s2]';
  if CheckBox3.Checked then tools.fontsize:='[s3]';
  if CheckBox4.Checked then tools.fontsize:='[s4]';
  if CheckBox5.Checked then tools.fontsize:='[s5]';
  close;
end;

procedure TSetFontSizeFrm.Button2Click(Sender: TObject);
begin
  tools.ButtonPress:=0;
  tools.fontsize:='';
  close;
end;

procedure TSetFontSizeFrm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    CheckBox2.Checked := False;
    CheckBox3.Checked := False;
    CheckBox4.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontSizeFrm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox3.Checked := False;
    CheckBox4.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontSizeFrm.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox2.Checked := False;
    CheckBox4.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontSizeFrm.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox2.Checked := False;
    CheckBox3.Checked := False;
    CheckBox5.Checked := False;
  end;
end;

procedure TSetFontSizeFrm.CheckBox5Click(Sender: TObject);
begin
  if CheckBox5.Checked then
  begin
    CheckBox1.Checked := False;
    CheckBox2.Checked := False;
    CheckBox3.Checked := False;
    CheckBox4.Checked := False;
  end;
end;

procedure TSetFontSizeFrm.FormCreate(Sender: TObject);
begin
  tools.ButtonPress:=0;
  tools.fontsize:='';
end;

end.

