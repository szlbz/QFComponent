unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ColorBox, ComCtrls, ComboEx, DBCtrls, EditBtn, Grids, QFComponent, Types,
  LCLIntf,LCLType, DBExtCtrls,TypInfo,Process;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button9: TButton;
    DateEdit1: TDateEdit;
    edit2: TEdit;
    Memo1: TMemo;
    Panel3: TPanel;
    Grid1: TStringGrid;
    edit3: TEdit;
    Button: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ColorBox1: TColorBox;
    edit1: TEdit;
    Label1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    QFGridPanelComponent1: TQFGridPanelComponent;
    QFHorizontalScrollingText1: TQFHorizontalScrollingText;
    QFRichView1: TQFRichView;
    ScrollingText1: TQFScrollingText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    edit4: TEdit;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
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
  PageControl1.TabIndex:=3;
  //QFRichView1.BackImagefile:='bg.jpg';
  ScrollingText1.BackImagefile:='bg.jpg';
  //QFHorizontalScrollingText1.BackImageFile:='bg.jpg';
  TabSheet2.Visible:=true;
  //UniConnection1.SpecificOptions.Values['SQL Server.Provider']:='prDirect';// .Add('SQL Server.Provider=prDirect');
  //UniConnection1.Connected:=true;
  //UniQuery1.Active:=true;
end;

procedure TForm1.ButtonClick(Sender: TObject);
begin
  showmessage('QFGridPanelComponent演示！');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   QFRichView1.RichEditor;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
   QFGridPanelComponent1.SaveQFConfig;//('g.json');
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  QFGridPanelComponent1.LoadQFConfig;//('g.json');
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  QFGridPanelComponent1.SetCellProper;
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

procedure TForm1.Button5Click(Sender: TObject);
begin
  QFRichView1.PageHeader;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  QFRichView1.PageFooter;
end;

procedure TForm1.Button9Click(Sender: TObject);
var i: int64;
begin
i := -5;
ShowMessage(IntToStr(i mod 8));
//OpenURL('http://bbs.2ccc.com');
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if checkbox1.Checked then ScrollingText1.Active:=true
  else ScrollingText1.Active:=false;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if checkbox2.Checked then QFHorizontalScrollingText1.Active:=true
  else QFHorizontalScrollingText1.Active:=false;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  if checkbox3.Checked then QFHorizontalScrollingText1.ShowBackImage:=true
  else QFHorizontalScrollingText1.ShowBackImage:=false;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  QFRichView1.Color:=ColorBox1.Colors[ColorBox1.ItemIndex];
end;

end.

