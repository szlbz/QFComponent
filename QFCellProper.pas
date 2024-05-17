unit QFCellProper;

interface             

uses
  LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Grids,PublicDefinition,FPCanvas;

type

  { TQFFormCellProp }

  { TQFCellProper }

  TQFCellProper = class(TForm)
    BevelCellType1: TBevel;
    BevelCellType2: TBevel;
    BevelCellType3: TBevel;
    BevelCellType4: TBevel;
    BevelCellType5: TBevel;
    BevelCellType6: TBevel;
    BtnCancel: TButton;
    BtnOk: TButton;
    BtnSetCellLineColor1: TButton;
    BtnSetFontColor: TButton;
    Button1: TButton;
    Button2: TButton;
    CbxCellType: TComboBox;
    CbxLineStyle: TComboBox;
    ChkBoxUnderLine: TCheckBox;
    ColEdit: TLabeledEdit;
    CellTextEdit: TLabeledEdit;
    ColMerge: TLabeledEdit;
    EditFontSize: TEdit;
    GbxFontPreview: TGroupBox;
    LabelFontColor: TLabel;
    LabelFontName: TLabel;
    LabelFontSize: TLabel;
    LabelFontStyle: TLabel;
    LbxFontName: TListBox;
    LbxFontSize: TListBox;
    LbxFontStyle: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelFontColor: TPanel;
    PanelFontPreview1: TPanel;
    RowMerge: TLabeledEdit;
    ColWidthEdit: TLabeledEdit;
    GroupBox1: TGroupBox;
    LeftLineStyle: TComboBox;
    RightLineStyle: TComboBox;
    RowEdit: TLabeledEdit;
    RowHeightEdit: TLabeledEdit;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    TopLineStyle: TComboBox;
    BottomLineStyle: TComboBox;
    DrawBottomLine: TCheckBox;
    DrawLeftLine: TCheckBox;
    DrawRightLine: TCheckBox;
    DrawTopLine: TCheckBox;
    ChkColSizing: TCheckBox;
    ChkRowSizing: TCheckBox;
    ComboBox1: TComboBox;
    LabelCellType1: TLabel;
    LabelControl1: TLabel;
    LabelControl2: TLabel;
    LabelControl3: TLabel;
    LineColor: TPanel;
    LabelCellType: TLabel;
    BevelCellType: TBevel;
    ColorDialogCellProp: TColorDialog;
    CbxHAlign: TComboBox;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnSetCellLineColor1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LbxFontStyleClick(Sender: TObject);
    procedure LbxFontSizeClick(Sender: TObject);
    procedure RightLineStyleChange(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure LbxFontNameClick(Sender: TObject);
    procedure ChkBoxUnderLineClick(Sender: TObject);
    procedure EditFontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure BtnSetFontColorClick(Sender: TObject);
    procedure CbxHAlignClick(Sender: TObject);
    procedure EditFontSizeChange(Sender: TObject);
  private
    row,col:integer;
    CellRange: TRect;
    procedure SetNumControls(Value: boolean);
    procedure GetFirstCellProp;
    procedure SetControlState;
  public
    GTable:Array of Array of TCell;
    ParentGrid: Pointer;
  end;

implementation
{$R *.lfm}

procedure TQFCellProper.BtnSetCellLineColor1Click(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    LineColor.Color := ColorDialogCellProp.Color;
    LineColor.Visible := True;
  end;
end;

procedure TQFCellProper.BtnCancelClick(Sender: TObject);
begin
  QFCellProperReturn:=false;
end;

procedure TQFCellProper.BtnOkClick(Sender: TObject);
begin
  if panel1.Enabled then
    RightLineStyleChange(self);//保存最后的修改
  QFCellProperReturn:=true;
end;

procedure TQFCellProper.Button1Click(Sender: TObject);
var err,cw,hh:integer;
  i,j:integer;
  RowCount,ColCount:integer;
begin
  val(RowEdit.Text,RowCount,err);
  val(ColEdit.Text,ColCount,err);
  val(RowHeightEdit.Text,hh,err);
  val(ColWidthEdit.Text,cw,err);
  //if (RowCount<>StringGrid1.RowCount) or (StringGrid1.ColCount<>ColCount) then
  //begin
    StringGrid1.RowCount:=RowCount;
    StringGrid1.ColCount:=ColCount;
    GTable:=nil;
    setlength(GTable,RowCount+1,ColCount+2);
    for i:=0 to RowCount-1 do
    begin
      for j:=0 to ColCount-1 do
      begin
        StringGrid1.Cells[j,i]:='';
        GTable[i,j].x:=j*cw;
        GTable[i,j].y:=i*hh;
        GTable[i,j].Width:=cw;
        GTable[i,j].Height:=hh;
        GTable[i,j].Visible:=true;
        GTable[i,j].ComponentName:='';
        GTable[i,j].ComponentDataFieldName:=nil;
        GTable[i,j].ComponentDataSource:=nil;
      end;
    end;
  //end;
end;

procedure TQFCellProper.Button2Click(Sender: TObject);
begin
  RightLineStyleChange(self);
  Panel1.Enabled:=false;
end;

procedure TQFCellProper.SetNumControls(Value: boolean);
begin
end;

procedure TQFCellProper.GetFirstCellProp;
begin
end;

procedure TQFCellProper.SetControlState;
begin
end;

procedure TQFCellProper.FormShow(Sender: TObject);
begin
  GetFirstCellProp;
  SetControlState;
end;

procedure TQFCellProper.FormCreate(Sender: TObject);
begin
  LbxFontName.Items.Assign(Screen.Fonts);
end;

procedure TQFCellProper.LbxFontNameClick(Sender: TObject);
begin
  with LbxFontName do
    PanelFontPreview1.Font.Name := Items[ItemIndex];
end;

procedure TQFCellProper.LbxFontStyleClick(Sender: TObject);
begin
  case LbxFontStyle.ItemIndex of
    0 : PanelFontPreview1.Font.Style := [];
    1 : PanelFontPreview1.Font.Style := [fsItalic];
    2 : PanelFontPreview1.Font.Style := [fsBold];
    3 : PanelFontPreview1.Font.Style := [fsBold, fsItalic];
  end;
end;

procedure TQFCellProper.LbxFontSizeClick(Sender: TObject);
begin
  with LbxFontSize do
  begin
    EditFontSize.Text := Items[ItemIndex];
    PanelFontPreview1.Font.Size := StrToInt(EditFontSize.Text);
  end;
end;

procedure TQFCellProper.RightLineStyleChange(Sender: TObject);
var
  tmp,err:integer;
begin
  if CbxHAlign.ItemIndex>=0 then
    GTable[Row,Col+1].Align:=CbxHAlign.ItemIndex+1;

  if CbxCellType.ItemIndex=0 then
    GTable[Row,Col+1].DispType:=0; //文字
  if CbxCellType.ItemIndex=1 then
    GTable[Row,Col+1].DispType:=1; //图像
  if CbxCellType.ItemIndex=2 then
    GTable[Row,Col+1].DispType:=5; //控件

  if ComboBox1.ItemIndex>-1 then
    GTable[Row,Col+1].ComponentName:=ComboBox1.Items[ComboBox1.ItemIndex];

  GTable[Row,Col+1].str:=CellTextEdit.Text;
  StringGrid1.cells[Col,Row]:=CellTextEdit.Text;
  val(RowMerge.Text,tmp,err);
  GTable[Row,Col+1].RowMerge:=tmp;
  val(ColMerge.Text,tmp,err);
  GTable[Row,Col+1].ColMerge:=tmp;

  if LbxFontName.ItemIndex>=0 then
    GTable[Row,Col+1].FontName:=LbxFontName.Items.ValueFromIndex[LbxFontName.ItemIndex];
  if LbxFontSize.itemindex>=0 then
  begin
    val(LbxFontSize.Items.ValueFromIndex[LbxFontSize.itemindex],tmp,err);
    GTable[Row,Col+1].FontSize:=tmp;
  end;
  GTable[Row,Col+1].FontColor:=PanelFontColor.Color;
  GTable[Row,Col+1].DrawBottom:=DrawBottomLine.Checked;
  GTable[Row,Col+1].DrawLeft:=DrawLeftLine.Checked;
  GTable[Row,Col+1].DrawRight:=DrawRightLine.Checked;
  GTable[Row,Col+1].DrawTop:=DrawTopLine.Checked;
  GTable[Row,Col+1].LeftLineStyle:=TFPPenStyle(LeftLineStyle.ItemIndex);
  GTable[Row,Col+1].RightLineStyle:=TFPPenStyle(RightLineStyle.ItemIndex);
  GTable[Row,Col+1].BottomLineStyle:=TFPPenStyle(BottomLineStyle.ItemIndex);
  GTable[Row,Col+1].TopLineStyle:=TFPPenStyle(TopLineStyle.ItemIndex);
  GTable[Row,Col+1].str:=StringGrid1.cells[Col,Row];
end;

procedure TQFCellProper.StringGrid1Click(Sender: TObject);
begin
  Panel1.Enabled:=true;
  row:=StringGrid1.Row;
  col:=StringGrid1.Col;
  ComboBox1.ItemIndex:=
     ComboBox1.Items.IndexOf(GTable[Row,Col+1].ComponentName);
  if GTable[Row,Col+1].Align=0 then GTable[Row,Col+1].Align:=2;
  if GTable[Row,Col+1].Align>0 then
    CbxHAlign.ItemIndex:=GTable[Row,Col+1].Align-1;

  if GTable[Row,Col+1].DispType=0 then
    CbxCellType.ItemIndex:=0;//文字
  if GTable[Row,Col+1].DispType=1 then
    CbxCellType.ItemIndex:=1;//图像
  if GTable[Row,Col+1].DispType=5 then
    CbxCellType.ItemIndex:=2;//控件
  ColMerge.Text:=GTable[Row,Col+1].ColMerge.ToString;
  RowMerge.Text:=GTable[Row,Col+1].RowMerge.ToString;
  EditFontSize.Text:=GTable[Row,Col+1].FontSize.ToString;
  LbxFontName.ItemIndex:=LbxFontName.Items.IndexOf(GTable[Row,Col+1].FontName);
  LbxFontSize.ItemIndex:=LbxFontSize.Items.IndexOf(GTable[Row,Col+1].FontSize.ToString);
  PanelFontColor.Color:=GTable[Row,Col+1].FontColor;
  PanelFontPreview1.Font.Color:=GTable[Row,Col+1].FontColor;
  DrawBottomLine.Checked:=GTable[Row,Col+1].DrawBottom;
  DrawLeftLine.Checked:=GTable[Row,Col+1].DrawLeft;
  DrawRightLine.Checked:=GTable[Row,Col+1].DrawRight;
  DrawTopLine.Checked:=GTable[Row,Col+1].DrawTop;
  LeftLineStyle.ItemIndex:=ord(GTable[Row,Col+1].LeftLineStyle);
  RightLineStyle.ItemIndex:=ord(GTable[Row,Col+1].RightLineStyle);
  BottomLineStyle.ItemIndex:=ord(GTable[Row,Col+1].BottomLineStyle);
  TopLineStyle.ItemIndex:=ord(GTable[Row,Col+1].TopLineStyle);
  PanelFontPreview1.Font.Name:=GTable[Row,Col+1].FontName;
  //PanelFontColor.Color:=GTable[Row,Col+1].Color;
  StatusBar1.Panels[0].Text:='行:'+row.ToString+'  列:'+col.ToString;
  StatusBar1.Panels[1].Text:=GTable[Row,Col+1].str;//StringGrid1.Cells[col,row];
  CellTextEdit.Text:=GTable[Row,Col+1].str;
end;

procedure TQFCellProper.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  TEdit((Sender as TUpDown).Associate).Enabled := True;
end;

procedure TQFCellProper.ChkBoxUnderLineClick(Sender: TObject);
begin
  if ChkBoxUnderLine.Checked then
    PanelFontPreview1.Font.Style := PanelFontPreview1.Font.Style + [fsUnderLine]
  else
    PanelFontPreview1.Font.Style := PanelFontPreview1.Font.Style - [fsUnderLine];
end;

procedure TQFCellProper.EditFontSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (integer(Key) in [$30..$39,VK_BACK,VK_INSERT,VK_END,VK_HOME]) then
    Key := #0;
end;

procedure TQFCellProper.EditFontSizeChange(Sender: TObject);
begin
  if (EditFontSize.Text <> '') and (StrToInt(EditFontSize.Text) > 409) then
    EditFontSize.Text := '409';
  if EditFontSize.Text = '' then
    PanelFontPreview1.Font.Size := 1
  else
    PanelFontPreview1.Font.Size := StrToInt(EditFontSize.Text);
end;

procedure TQFCellProper.BtnSetFontColorClick(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    PanelFontPreview1.Font.Color := ColorDialogCellProp.Color;
    PanelFontColor.Color := ColorDialogCellProp.Color;
    PanelFontColor.Visible := True;
  end;
end;

procedure TQFCellProper.CbxHAlignClick(Sender: TObject);
begin
  if CbxHAlign.ItemIndex < 0 then
    CbxHAlign.ItemIndex := 0;
end;

end.
