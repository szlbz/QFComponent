unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ColorBox, QFComponent;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    ColorBox1: TColorBox;
    Label1: TLabel;
    QFRichView1: TQFRichView;
    ScrollingText1: TQFScrollingText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure UniQuery1nameGetText(Sender: TField; var aText: string;
    //  DisplayText: Boolean);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  QFRichView1.BackImagefile:='bg.jpg';
  ScrollingText1.BackImagefile:='bg.jpg';
  //UniConnection1.SpecificOptions.Values['SQL Server.Provider']:='prDirect';// .Add('SQL Server.Provider=prDirect');
  //UniConnection1.Connected:=true;
  //UniQuery1.Active:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   QFRichView1.RichEditor;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  QFRichView1.OpenFile('');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  QFRichView1.SavePicture('QFRichView_demo.jpg');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  QFRichView1.Print;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if checkbox1.Checked then ScrollingText1.Active:=true
  else ScrollingText1.Active:=false;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  QFRichView1.Color:=ColorBox1.Colors[ColorBox1.ItemIndex];
end;

//procedure TForm1.UniQuery1nameGetText(Sender: TField; var aText: string;
//  DisplayText: Boolean);
//begin
//  aText:= CP936ToUTF8(Sender.Value);
//end;

end.

