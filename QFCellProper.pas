unit QFCellProper;

interface             

uses
  LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Grids, ExtDlgs, PublicUnit,fpjson, FPCanvas;

type

  { TQFCellProper }

  TQFCellProper = class(TForm)
    BackImageFile: TLabeledEdit;
    BevelCellType1: TBevel;
    BevelCellType2: TBevel;
    BevelCellType3: TBevel;
    BevelCellType4: TBevel;
    BevelCellType5: TBevel;
    BevelCellType6: TBevel;
    BevelCellType7: TBevel;
    BtnCancel: TButton;
    BtnOk: TButton;
    BtnSetCellLineColor1: TButton;
    BtnSetFontColor: TButton;
    BtnSetFontColor1: TButton;
    BtnSetFontColor2: TButton;
    BtnSetFontColor3: TButton;
    BtnSetFontColor4: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CbxCellType: TComboBox;
    CbxLineStyle: TComboBox;
    CellGapEdit: TLabeledEdit;
    CheckBox1: TCheckBox;
    ChkBoxUnderLine: TCheckBox;
    CellTextEdit: TLabeledEdit;
    ChkColSizing: TCheckBox;
    ChkRowSizing: TCheckBox;
    ColEdit: TLabeledEdit;
    ColMerge: TLabeledEdit;
    ColWidthEdit: TLabeledEdit;
    Edit1: TEdit;
    EditFocusColor: TPanel;
    EditFontFocusColor: TPanel;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    TextCellColor: TPanel;
    EditFontSize: TEdit;
    GapEdit: TLabeledEdit;
    GbxFontPreview: TGroupBox;
    LabelFontColor: TLabel;
    LabelFontColor1: TLabel;
    LabelFontColor2: TLabel;
    LabelFontColor3: TLabel;
    LabelFontName: TLabel;
    LabelFontSize: TLabel;
    LabelFontStyle: TLabel;
    LbxFontName: TListBox;
    LbxFontSize: TListBox;
    LbxFontStyle: TListBox;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelFontColor: TPanel;
    PanelFontPreview1: TPanel;
    RowEdit: TLabeledEdit;
    RowHeightEdit: TLabeledEdit;
    RowMerge: TLabeledEdit;
    GroupBox1: TGroupBox;
    LeftLineStyle: TComboBox;
    RightLineStyle: TComboBox;
    ShowBackImage: TCheckBox;
    StatusBar1: TStatusBar;
    TableBorder: TCheckBox;
    TopLineStyle: TComboBox;
    BottomLineStyle: TComboBox;
    DrawBottomLine: TCheckBox;
    DrawLeftLine: TCheckBox;
    DrawRightLine: TCheckBox;
    DrawTopLine: TCheckBox;
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
    procedure BtnSetFontColor1Click(Sender: TObject);
    procedure BtnSetFontColor2Click(Sender: TObject);
    procedure BtnSetFontColor3Click(Sender: TObject);
    procedure BtnSetFontColor4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LbxFontStyleClick(Sender: TObject);
    procedure LbxFontSizeClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure LbxFontNameClick(Sender: TObject);
    procedure ChkBoxUnderLineClick(Sender: TObject);
    procedure EditFontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure BtnSetFontColorClick(Sender: TObject);
    procedure CbxHAlignClick(Sender: TObject);
    procedure EditFontSizeChange(Sender: TObject);
    procedure Click(Sender: TObject);
  private
    CellRange: TRect;
    procedure SetNumControls(Value: boolean);
    procedure GetFirstCellProp;
    procedure SetControlState;
  public
    //ComponentStates:Boolean;
    FHeight:int64;
    FWidth :int64;
    FShowBackImage:boolean;
    FBackImageFile:string;
    FEditFontFocusColor:TColor;
    FEditFocusColor:TColor;
    FBorder:Boolean;
    FGap:integer;
    FColWidth:integer;
    FRowHeight:integer;
    FTableWidth:integer;
    FTableHeight:integer;
    FCellLineStyle:TFPPenStyle;
    FColSizing:Boolean;
    FRowSizing:Boolean;
    FCellLineColor:TColor;

    FRowCount,FColCount:Integer;
    oldwidth,oldheight:Integer;
    row,col:integer;
    oldColMerge,oldRowMerge:integer;
    GTable:Array of Array of TCell;
    ParentGrid: Pointer;
    jsonstr:String;
    function GetJSON:String;
  end;

implementation

uses
  QFComponent;

var
QFGridPanel: TQFGridPanelComponent;

{$R *.lfm}

function TQFCellProper.GetJSON:String;
VAR
  jsonRoot, jsoncomp, jsonGrid, jsonRow,  jsonParamObj: TJSONObject;
  jsonCol: TJSONArray;
  jData,jItem : TJSONData;
  i,j: Integer;
  //savejsonfile: TStringList ;
begin
  //创建一个新的JSON对象来写入数据
  jsonRoot := TJSONObject.Create;
  jsonRoot.Add('ConfigName', utf8toansi('QFGridPanelComponentConfig'));
  jsonRoot.Add('Version', Version);
  jsonGrid := TJSONObject.Create;
  jsonRoot.Add(QFGridPanel.Name, jsonGrid);//添加QFGridPanelComponent

  jsonGrid.Add('ComponentName', QFGridPanel.Name);
  jsonGrid.Add('FRowcount', FRowCount);
  jsonGrid.Add('FColcount', FColCount);
  jsonGrid.Add('FCellLineColor', FCellLineColor);
  jsonGrid.Add('FColSizing', FColSizing);
  jsonGrid.Add('FRowSizing', FRowSizing);
  jsonGrid.Add('FCellLineStyle', ord(FCellLineStyle));
  jsonGrid.Add('FColWidth', FColWidth);
  jsonGrid.Add('FRowHeight', FRowHeight);
  jsonGrid.Add('FTableWidth', FTableWidth);
  jsonGrid.Add('FTableHeight', FTableHeight);
  jsonGrid.Add('FGap', FGap);
  jsonGrid.Add('FBorder', FBorder);
  jsonGrid.Add('FEditFontFocusColor', FEditFontFocusColor);
  jsonGrid.Add('FEditFocusColor', FEditFocusColor);
  jsonGrid.Add('FBackImageFile', FBackImageFile);
  jsonGrid.Add('FShowBackImage', FShowBackImage);

  for i := 0 to FRowCount-1 do
  begin
    jsonRow := TJSONObject.Create;
    jsonGrid.Add(Format('row%d', [i]), jsonRow);
    for j := 0 to FColCount do
    begin
      jsonCol := TJSONArray.Create;
      jsonRow.Add(Format('col%d', [j]), jsonCol);

      jsonParamObj := TJSONObject.Create;
      jsonParamObj.Add('x', GTable[i,j].x);
      jsonParamObj.Add('y', GTable[i,j].y);
      jsonParamObj.Add('Gap', GTable[i,j].Gap);
      jsonParamObj.Add('Width', GTable[i,j].Width);
      jsonParamObj.Add('Height', GTable[i,j].Height);
      jsonParamObj.Add('ColMerge', GTable[i,j].ColMerge);
      jsonParamObj.Add('RowMerge', GTable[i,j].RowMerge);
      jsonParamObj.Add('DispType', ord(GTable[i,j].DispType));
      jsonParamObj.Add('str', GTable[i,j].str);
      jsonParamObj.Add('Color', GTable[i,j].Color);
      jsonParamObj.Add('Align', ord(GTable[i,j].Align));
      jsonParamObj.Add('FontName', GTable[i,j].FontName);
      jsonParamObj.Add('FontSize', GTable[i,j].FontSize);
      jsonParamObj.Add('FontStyle', ord(GTable[i,j].FontStyle));
      jsonParamObj.Add('FontColor', GTable[i,j].FontColor);
      jsonParamObj.Add('ComponentType', GTable[i,j].ComponentType);
      jsonParamObj.Add('ComponentName', GTable[i,j].ComponentName);
      jsonParamObj.Add('Visible', GTable[i,j].Visible);
      jsonParamObj.Add('DrawTop', GTable[i,j].DrawTop);
      jsonParamObj.Add('DrawLeft', GTable[i,j].DrawLeft);
      jsonParamObj.Add('DrawBottom', GTable[i,j].DrawBottom);
      jsonParamObj.Add('DrawRight', GTable[i,j].DrawRight);
      jsonParamObj.Add('LineStyle', ord(GTable[i,j].LineStyle));
      jsonParamObj.Add('TopLineStyle', ord(GTable[i,j].TopLineStyle));
      jsonParamObj.Add('LeftLineStyle', ord(GTable[i,j].LeftLineStyle));
      jsonParamObj.Add('BottomLineStyle', ord(GTable[i,j].BottomLineStyle));
      jsonParamObj.Add('RightLineStyle', ord(GTable[i,j].RightLineStyle));
      jsonParamObj.Add('TextCellColor', GTable[i,j].TextCellColor);

      jsonCol.Add(jsonParamObj);
      jsonParamObj:=nil;
      jsonCol:=nil;
    end;
  end;
  Result:=jsonRoot.FormatJSON;
//  savejsonfile:= TStringList.Create;
//  savejsonfile.Add(Result);
//  savejsonfile.SaveToFile('1.json');
  jsonGrid:=nil;
  jsonRow:=nil;
  jsonRoot:=nil;
  jsonstr:=Result;
end;

procedure TQFCellProper.BtnSetCellLineColor1Click(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    LineColor.Color := ColorDialogCellProp.Color;
    LineColor.Visible := True;
  end;
end;

procedure TQFCellProper.BtnSetFontColor1Click(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    EditFocusColor.Color := ColorDialogCellProp.Color;
  end;
end;

procedure TQFCellProper.BtnSetFontColor2Click(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    EditFontFocusColor.Color := ColorDialogCellProp.Color;
  end;
end;

procedure TQFCellProper.BtnSetFontColor3Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    BackImageFile.Text:=ExtractFileName(OpenPictureDialog1.FileName);
  end;
end;

procedure TQFCellProper.BtnSetFontColor4Click(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    TextCellColor.Color := ColorDialogCellProp.Color;
  end;
end;

procedure TQFCellProper.BtnCancelClick(Sender: TObject);
begin
  self.ModalResult:=mrCancel;
end;

procedure TQFCellProper.BtnOkClick(Sender: TObject);
begin
  if panel1.Enabled then
    Button2Click(self);//保存最后的修改
  self.ModalResult:=mrOk;
end;

procedure TQFCellProper.Button1Click(Sender: TObject);
var err,cw,hh:integer;
  i,j,jj:integer;
begin
  //建新表格
  val(RowEdit.Text,FRowCount,err);
  val(ColEdit.Text,FColCount,err);

  hh:=0;
  cw:=0;
  GTable:=nil;
  setlength(GTable,FRowCount+1,FColCount+2);
  for i:=0 to FRowCount-1 do
  begin
    for j:=0 to FColCount do
    begin
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
  QFGridPanel.Config.Text:=GetJSON;
  QFGridPanel.Refresh;
  QFGridPanel.RedrawTable;
end;

procedure TQFCellProper.Button2Click(Sender: TObject);
var
  tmp,err:integer;
begin
  Panel1.Enabled:=false;
  if CbxHAlign.ItemIndex>=0 then
    GTable[Row,Col+1].Align:=CellAlign(CbxHAlign.ItemIndex+1);

  if CbxCellType.ItemIndex=0 then
    GTable[Row,Col+1].DispType:=dtText; //文字
  if CbxCellType.ItemIndex=1 then
    GTable[Row,Col+1].DispType:=dtPict; //图像
  if CbxCellType.ItemIndex=2 then
    GTable[Row,Col+1].DispType:=dtComponent; //控件
  //保存文字
  GTable[Row,Col+1].str:=CellTextEdit.Text;
  //保存文字
  if (ComboBox1.ItemIndex>-1) and (CbxCellType.ItemIndex=2) then
    GTable[Row,Col+1].ComponentName:=ComboBox1.Items[ComboBox1.ItemIndex]
  else
    GTable[Row,Col+1].ComponentName:='';

  val(CellGapEdit.Text,tmp,err);
  GTable[Row,Col+1].Gap:=tmp;

  val(RowMerge.Text,tmp,err);
  GTable[Row,Col+1].RowMerge:=tmp;
  if tmp<>oldRowMerge then
    GTable[Row,Col+1].Height:=0;

  val(ColMerge.Text,tmp,err);
  GTable[Row,Col+1].ColMerge:=tmp;
  if tmp<>oldColMerge then
    GTable[Row,Col+1].Width:=0;

  if LbxFontName.ItemIndex>=0 then
    GTable[Row,Col+1].FontName:=LbxFontName.Items.ValueFromIndex[LbxFontName.ItemIndex];
  if LbxFontSize.itemindex>=0 then
  begin
    val(LbxFontSize.Items.ValueFromIndex[LbxFontSize.itemindex],tmp,err);
    GTable[Row,Col+1].FontSize:=tmp;
  end;
  GTable[Row,Col+1].TextCellColor:=TextCellColor.Color;
  GTable[Row,Col+1].FontColor:=PanelFontColor.Color;
  GTable[Row,Col+1].DrawBottom:=DrawBottomLine.Checked;
  GTable[Row,Col+1].DrawLeft:=DrawLeftLine.Checked;
  GTable[Row,Col+1].DrawRight:=DrawRightLine.Checked;
  GTable[Row,Col+1].DrawTop:=DrawTopLine.Checked;
  GTable[Row,Col+1].LeftLineStyle:=TFPPenStyle(LeftLineStyle.ItemIndex);
  GTable[Row,Col+1].RightLineStyle:=TFPPenStyle(RightLineStyle.ItemIndex);
  GTable[Row,Col+1].BottomLineStyle:=TFPPenStyle(BottomLineStyle.ItemIndex);
  GTable[Row,Col+1].TopLineStyle:=TFPPenStyle(TopLineStyle.ItemIndex);
  if col+1>1 then
  begin
    GTable[Row,Col].RightLineStyle:=GTable[Row,Col+1].LeftLineStyle;
    GTable[Row,Col+2].LeftLineStyle:=GTable[Row,Col+1].RightLineStyle;
  end;
  if Row>1 then
  begin
    GTable[Row-1,Col+1].BottomLineStyle:=GTable[Row,Col+1].TopLineStyle;
    GTable[Row+1,Col+1].TopLineStyle:=GTable[Row,Col+1].BottomLineStyle;
    //psSolid
  end;
  QFGridPanel.Config.Text:=GetJSON;
  QFGridPanel.Refresh;
  QFGridPanel.RedrawTable;
end;

procedure TQFCellProper.Button3Click(Sender: TObject);
var
  newwidth,err,i,j:Integer;
begin
  val(Edit1.Text,newwidth,err);
  if CheckBox1.Checked then
  begin
    for i:=1 to FRowCount do
    begin
      GTable[i-1,col+1].Width:=NewWidth;
      if newwidth>oldwidth then
      begin
        GTable[i-1,col+2].x:=GTable[i-1,col+2].x+(NewWidth-oldwidth);
        GTable[i-1,col+2].Width:=GTable[i-1,col+2].Width-(NewWidth-oldwidth)
      end
      else
      begin
        GTable[i-1,col+2].x:=GTable[i-1,col+2].x-(oldwidth-NewWidth);
        GTable[i-1,col+2].Width:=GTable[i-1,col+2].Width+(oldwidth-NewWidth);
      end;
    end;
  end
  else
  begin
    i:=1;
    GTable[row+i-1,col+1].Width:=NewWidth;
    if newwidth>oldwidth then
    begin
      GTable[row+i-1,col+2].x:=GTable[row+i-1,col+2].x+(NewWidth-oldwidth);
      GTable[row+i-1,col+2].Width:=GTable[row+i-1,col+2].Width-(NewWidth-oldwidth)
    end
    else
    begin
      GTable[row+i-1,col+2].x:=GTable[row+i-1,col+2].x-(oldwidth-NewWidth);
      GTable[row+i-1,col+2].Width:=GTable[row+i-1,col+2].Width+(oldwidth-NewWidth);
    end;
  end;
  QFGridPanel.Config.Text:=GetJSON;
  QFGridPanel.Refresh;
  QFGridPanel.RedrawTable;
end;

procedure TQFCellProper.Button4Click(Sender: TObject);
begin
  QFGridPanel.Config.Text:=GetJSON;
  QFGridPanel.Refresh;
  QFGridPanel.RedrawTable;
end;

procedure TQFCellProper.ComboBox1Change(Sender: TObject);
begin
  if combobox1.ItemIndex>-1 then
    CbxCellType.ItemIndex:=2;
end;

procedure TQFCellProper.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
 FreeAndNil(QFGridPanel);
end;

procedure TQFCellProper.FormResize(Sender: TObject);
begin
  QFGridPanel.Width:=ScrollBox1.Width+100;
  QFGridPanel.Height:=ScrollBox1.Height+100;
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
  QFGridPanel.Config.Text:=GetJSON;
  QFGridPanel.Refresh;
  QFGridPanel.RedrawTable;
end;


procedure TQFCellProper.FormCreate(Sender: TObject);
begin
  LbxFontName.Items.Assign(Screen.Fonts);
  QFGridPanel:=TQFGridPanelComponent.Create(ScrollBox1);
  //if ComponentStates then
  //begin
  QFGridPanel.Width:=ScrollBox1.Width+100;
  QFGridPanel.Height:=ScrollBox1.Height+100;
  QFGridPanel.MouseFollow:=False;
  //end
  //else
  //begin
  //  QFGridPanel.Align:=alClient;
  //end;
  QFGridPanel.Name:='QFGridPanel';
  QFGridPanel.HideMenu:=True;
  QFGridPanel.RowSizing:=False;
  QFGridPanel.ColSizing:=False;
  QFGridPanel.OnClick:=@Click;
end;

procedure TQFCellProper.Click(Sender: TObject);
begin
  Panel1.Enabled:=true;
  row:=QFGridPanel.GetSelectRow;
  col:=QFGridPanel.GetSelectCol-1;
  ComboBox1.ItemIndex:=
     ComboBox1.Items.IndexOf(GTable[Row,Col+1].ComponentName);
  if GTable[Row,Col+1].Align=calNone then GTable[Row,Col+1].Align:=calClient;
  if GTable[Row,Col+1].Align>calNone then
    CbxHAlign.ItemIndex:=ord(GTable[Row,Col+1].Align)-1;

  if GTable[Row,Col+1].DispType=dtText then
    CbxCellType.ItemIndex:=0;//文字
  if GTable[Row,Col+1].DispType=dtPict then
    CbxCellType.ItemIndex:=1;//图像
  if GTable[Row,Col+1].DispType=dtComponent then
    CbxCellType.ItemIndex:=2;//控件
  Edit1.Text:=GTable[row,col+1].Width.ToString;
  oldwidth:=GTable[row,col+1].Width;
  oldheight:=GTable[row,col+1].Height;
  oldColMerge:=GTable[Row,Col+1].ColMerge;
  oldRowMerge:=GTable[Row,Col+1].RowMerge;
  ColMerge.Text:=GTable[Row,Col+1].ColMerge.ToString;
  RowMerge.Text:=GTable[Row,Col+1].RowMerge.ToString;
  CellGapEdit.Text:=GTable[Row,Col+1].Gap.ToString;
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
  if GTable[Row,Col+1].FontName<>'' then
    PanelFontPreview1.Font.Name:=GTable[Row,Col+1].FontName; //2026
  if GTable[Row,Col+1].TextCellColor<> ClBlack then
  begin
    TextCellColor.Color:=GTable[Row,Col+1].TextCellColor;
    PanelFontPreview1.Color:=GTable[Row,Col+1].TextCellColor;
  end
  else
  begin
    TextCellColor.Color:=clWhite;
    PanelFontPreview1.Color:=clWhite;
  end;
  StatusBar1.Panels[0].Text:='行:'+(row+1).ToString+'  列:'+(col+1).ToString;
  StatusBar1.Panels[1].Text:=GTable[Row,Col+1].str;
  CellTextEdit.Text:=GTable[Row,Col+1].str;
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
