unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  QFComponent;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    QFRichView1: TQFRichView;
    ScrollingText1: TQFScrollingText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  ScrollingText1.Active:=true;
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

//procedure TForm1.UniQuery1nameGetText(Sender: TField; var aText: string;
//  DisplayText: Boolean);
//begin
//  aText:= CP936ToUTF8(Sender.Value);
//end;

end.

