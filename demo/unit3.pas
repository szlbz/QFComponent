unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ColorBox, ComCtrls, ComboEx, DBCtrls, EditBtn, QFComponent, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    DateEdit1: TDateEdit;
    eeee: TEdit;
    Label2: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel3: TPanel;
    tel: TEdit;
    name3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ColorBox1: TColorBox;
    name1: TEdit;
    Label1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    QFGridPanelComponent1: TQFGridPanelComponent;
    QFHorizontalScrollingText1: TQFHorizontalScrollingText;
    QFRichView1: TQFRichView;
    ScrollingText1: TQFScrollingText;
    TabControl1: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    name2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit1KeyPress(Sender: TObject; var Key: char);
    procedure LabeledEdit2KeyPress(Sender: TObject; var Key: char);
    procedure TabControl1Change(Sender: TObject);
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
  memo2.Lines.Assign(QFGridPanelComponent1.Lines);
  PageControl1.TabIndex:=3;
  //QFRichView1.BackImagefile:='bg.jpg';
  ScrollingText1.BackImagefile:='bg.jpg';
  //QFHorizontalScrollingText1.BackImageFile:='bg.jpg';
  TabSheet2.Visible:=true;
  //UniConnection1.SpecificOptions.Values['SQL Server.Provider']:='prDirect';// .Add('SQL Server.Provider=prDirect');
  //UniConnection1.Connected:=true;
  //UniQuery1.Active:=true;
end;

procedure TForm1.LabeledEdit1KeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then Button7Click(self);
end;

procedure TForm1.LabeledEdit2KeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then Button7Click(self);
end;

procedure TForm1.TabControl1Change(Sender: TObject);
begin
  if TabControl1.TabIndex=0 then
  begin
    PageControl1.TabIndex:=0;
  end
  else if TabControl1.TabIndex=1 then
  begin
    PageControl1.TabIndex:=1;
  end
  else if TabControl1.TabIndex=2 then
  begin
    PageControl1.TabIndex:=2;
  end
  else if TabControl1.TabIndex=3 then
  begin
    PageControl1.TabIndex:=3;
  end;
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

procedure TForm1.Button5Click(Sender: TObject);
begin
  QFRichView1.PageHeader;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  QFRichView1.PageFooter;
end;

procedure TForm1.Button7Click(Sender: TObject);
var e,h:integer;
  gh:integer;
begin
  val(LabeledEdit1.text,h,e);
  val(LabeledEdit2.text,gh,e);
  QFGridPanelComponent1.Lines.Clear;
  QFGridPanelComponent1.RowHeight:=h;
  QFGridPanelComponent1.Height:=gh;
  QFGridPanelComponent1.Lines.Assign(memo2.Lines);
  QFGridPanelComponent1.Refresh;
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

//procedure TForm1.UniQuery1nameGetText(Sender: TField; var aText: string;
//  DisplayText: Boolean);
//begin
//  aText:= CP936ToUTF8(Sender.Value);
//end;

end.

