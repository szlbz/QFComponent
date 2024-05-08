unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ColorBox, ComCtrls, ComboEx, DBCtrls, EditBtn, Grids, DBGrids, QFComponent,
  Types, fpjson, jsonparser, TypInfo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button10: TButton;
    Button11: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox4: TCheckBox;
    DateEdit1: TDateEdit;
    eeee: TEdit;
    Grid1: TStringGrid;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    name1: TEdit;
    name2: TEdit;
    name3: TButton;
    Panel3: TPanel;
    QFGridPanelComponent1: TQFGridPanelComponent;
    tel: TEdit;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit1KeyPress(Sender: TObject; var Key: char);
    procedure LabeledEdit2KeyPress(Sender: TObject; var Key: char);
    procedure name3Click(Sender: TObject);
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
end;

procedure TForm1.LabeledEdit1KeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then Button7Click(self);
end;

procedure TForm1.LabeledEdit2KeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then Button7Click(self);
end;

procedure TForm1.name3Click(Sender: TObject);
begin
  showmessage('QFGridPanelComponent演示！');
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
   QFGridPanelComponent1.SaveQFConfig;//('g.json');
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  QFGridPanelComponent1.LoadQFConfig;//('g.json');
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

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    QFGridPanelComponent1.Border:=true
  else
    QFGridPanelComponent1.Border:=false;
  QFGridPanelComponent1.Refresh;
end;

//procedure TForm1.UniQuery1nameGetText(Sender: TField; var aText: string;
//  DisplayText: Boolean);
//begin
//  aText:= CP936ToUTF8(Sender.Value);
//end;

end.

