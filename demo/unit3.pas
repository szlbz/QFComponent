unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ColorBox, ComCtrls, ComboEx, DBCtrls, EditBtn, Grids, QFComponent, Types,
  fpjson, jsonparser,TypInfo;

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
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox4: TCheckBox;
    DateEdit1: TDateEdit;
    edit2: TEdit;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Memo1: TMemo;
    Memo2: TMemo;
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
    TabControl1: TTabControl;
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
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit1KeyPress(Sender: TObject; var Key: char);
    procedure LabeledEdit2KeyPress(Sender: TObject; var Key: char);
    procedure ButtonClick(Sender: TObject);
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
  TabControl1.TabIndex:=3;
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

procedure TForm1.ButtonClick(Sender: TObject);
begin
  showmessage('QFGridPanelComponent演示！');
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

procedure TForm1.Button8Click(Sender: TObject);
(*
{
  "rowcount": "2",
  "colcount": "2",
  "grid": {
    "row1": {
        "col1": [{
	        "参数1": "123"
        }, {
	        "参数2": "123"
        }],
        "col2": [{
	        "参数1": "123"
        }, {
	        "参数2": "123"
        }]
    },
    "row2": {
	"col1": [{
		"参数1": "123"
	}, {
		"参数2": "123"
	}],
	"col2": [{
		"参数1": "123"
	}, {
		"参数2": "123"
	}]
    }
  }
}
*)

  procedure ReadJSON(const JSONs: string);
  var
    jsonRoot, jsonGrid, jsonRow, jsonCol, jsonParams, jsonParamObj: TJSONObject;
    jsonParamsArray: TJSONArray;
    jsonParser :TJSONParser;
    jData:TJSONData;
    //jsonParam: TJSONPair;
    jsonString,rowCount,colCount:string ;
    i, j,k: Integer;
  begin
    // 假设jsonString包含上面提供的JSON数据
    jsonString := '{ "rowcount" : "2", "colcount" : "2", "grid" : { "row1" : { "col1" : [{ "参数1" : "123" }, { "参数2" : "123" }], "col2" : [{ "参数1" : "123" }, { "参数2" : "123" }] }, "row2" : { "col1" : [{ "参数1" : "123" }, { "参数2" : "123" }], "col2" : [{ "参数1" : "123" }, { "参数2" : "123" }] } } }'; // 替换为实际的JSON字符串
    jData:=GetJSON(jsonString);    //获取json数据
    jsonRoot:=TJSONObject(jData);    //json数据结构化一

    jsonParser := TJSONParser.Create;
    //jsonRoot := jsonParser.Parse(jsonString).AsJSON` as TJSONObject;
    jsonParser.Free;

    // 读取JSON数据
    rowCount := jsonRoot.Get('rowcount') ;
    colCount := jsonRoot.Get('colcount');

    jsonGrid :=jsonRoot.get('grid',jsonGrid);
    for i := 1 to 2 do // 假设有两行数据
    begin
      jsonRow := jsonGrid.Get(Format('row%d', [i]),jsonRow);
      for j := 1 to 2 do // 假设每行有两列数据
      begin
        jsonCol := jsonRow.Get(Format('col%d', [j]),jsonCol);
        //jsonParamsArray := jsonCol.Get('parameters');
        //for k := 0 to jsonParamsArray .Size - 1 do
        //begin
        //  jsonParamObj := jsonParamsArray.Get(k);
        //  for jsonCol in jsonParamObj do
        //  begin
        //    WriteLn(Format('键: %s, 值: %s', [jsonCol.JsonString, jsonParam.JsonValue.AsString]));
        //  end;
        //end;
      end;
    end;

    // 释放已解析的JSON对象
    jsonRoot.Free;
  end;

  procedure WriteJSON;
  VAR
  jsonRoot, jsonGrid, jsonRow,  jsonParamObj: TJSONObject;
  jsonCol: TJSONArray;
  i, j,paramIndex: Integer;
  begin
    // 现在，我们创建一个新的JSON对象来写入数据
    jsonRoot := TJSONObject.Create;
    jsonRoot.Add('rowcount', '2');
    jsonRoot.Add('colcount', '2');
    jsonGrid := TJSONObject.Create;
    jsonRoot.Add('grid', jsonGrid);

    for i := 1 to 4 do // 创建两行数据
    begin
      jsonRow := TJSONObject.Create;
      jsonGrid.Add(Format('row%d', [i]), jsonRow);
      for j := 1 to 3 do // 创建每行的两列数据
      begin
        jsonCol := TJSONArray.Create;
        jsonRow.Add(Format('col%d', [j]), jsonCol);

        // 添加参数对象到数组
        for paramIndex := 1 to 5 do // 假设每列有两个参数
        begin
          jsonParamObj := TJSONObject.Create;
          jsonParamObj.Add(Format('参数%d', [paramIndex]), TJSONString.Create('123'));
          jsonCol.Add(jsonParamObj);
        end;
      end;
    end;
    memo2.Text:=  jsonRoot.AsJSON;
  end;
  begin
   //ReadJSON('{"grid": {"row1": {"col1": ["参数1", ..., "参数n"], "coln": ["参数1", ..., "参数n"]}, "rown": {"col1": ["参数1", ..., "参数n"], "coln": ["参数1", ..., "参数n"]}}}');

   ReadJSON('');
    // 写入JSON数据

   WriteJSON;

end;

procedure TForm1.Button9Click(Sender: TObject);
  procedure JSONItems(Info: TStrings);
  var
    jData : TJSONData;
    jItem,kItem,litem,mitem,nitem,tmp : TJSONData;
    jsonitem:TJSONArray;
    i, j,k,l,m,n: Integer;
    object_name, field_name, field_value, object_type, object_items: String;
  begin
    jData := GetJSON(utf8toansi(
    '{ "rowcount" : "2", "colcount" : "2", "grid" : { "row1" : { "col1" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col2" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col3" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }] }, "row2" : { "col1" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col2" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col3" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }] }, "row3" : { "col1" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col2" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col3" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }] }, "row4" : { "col1" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col2" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }], "col3" : [{ "参数1" : "123" }, { "参数2" : "123" }, { "参数3" : "123" }, { "参数4" : "123" }, { "参数5" : "123" }] } } }'
    {
    '{ "rowcount" : "2", "colcount" : "2",'+
    ' "grid" : {'+
    ' "row1" : { "row1col1" : [{ "参数1" : "123" }, { "参数2" : "1234" },'+
                              '{ "参数1" : "123" }, { "参数2" : "1234" }],'+
               ' "row1col2" : [{ "参数3" : "4123" }, { "参数4" : "41234" },'+
                              '{ "参数1" : "123" }, { "参数2" : "1234" }] },'+
    ' "row2" : { "row2col1" : [{ "参数5" : "1234" }, { "参数6" : "1235" },'+
                              '{ "参数1" : "123" }, { "参数2" : "1234" }],'+
               ' "row2col2" : [{ "参数7" : "1236" }, { "参数8" : "1237" },'+
                              '{ "参数1" : "123" }, { "参数2" : "1234" }] }}  }'
}
    ));

    for i := 0 to jData.Count - 1 do
    begin
      jItem := jData.Items[i];
      object_name := TJSONObject(jData).Names[i];
      Info.Append(i.ToString+'、object name: ' + object_name);
      for j := 0 to jItem.Count - 1 do  //grid
      begin
        kItem:=jItem.Items[j];
        for k := 0 to kItem.Count - 1 do //row
        begin
          lItem:=kItem.Items[k];
          Info.Append(j.ToString+'、row：' + TJSONObject(kItem).Names[k]);
          for l := 0 to lItem.Count - 1 do   //col
          begin
            field_name :=TJSONObject(lItem.Items[l]).Names[0];
            field_value := lItem.Items[l].FindPath(TJSONObject(lItem.Items[l]).Names[0]).AsString;
            Info.Append(field_name + '|' + field_value);
          end;
        end;
      end;
    end;

    jData.Free;
  end;
 begin
    JSONItems(memo2.Lines)
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

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    QFGridPanelComponent1.Border:=true
  else
    QFGridPanelComponent1.Border:=false;
  QFGridPanelComponent1.Refresh;
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

