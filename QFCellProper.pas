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
    BtnSetCellLineColor1: TButton;
    Button1: TButton;
    CbxCellType: TComboBox;
    CbxLineStyle: TComboBox;
    ColEdit: TLabeledEdit;
    ColWidthEdit: TLabeledEdit;
    LeftLineStyle: TComboBox;
    RightLineStyle: TComboBox;
    RowEdit: TLabeledEdit;
    RowHeightEdit: TLabeledEdit;
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
    PageCtlCellProp: TPageControl;
    LineColor: TPanel;
    TabSheetCellType: TTabSheet;
    TabSheetFont: TTabSheet;
    BtnOk: TButton;
    BtnCancel: TButton;
    LabelFontName: TLabel;
    LabelCellType: TLabel;
    BevelCellType: TBevel;
    LabelFontStyle: TLabel;
    LabelFontSize: TLabel;
    LbxFontName: TListBox;
    LbxFontStyle: TListBox;
    LbxFontSize: TListBox;
    LabelFontColor: TLabel;
    GbxFontPreview: TGroupBox;
    PanelFontPreview1: TPanel;
    ChkBoxUnderLine: TCheckBox;
    PanelFontColor: TPanel;
    BtnSetFontColor: TButton;
    EditFontSize: TEdit;
    ColorDialogCellProp: TColorDialog;
    CbxHAlign: TComboBox;
    ChkBoxMerge: TCheckBox;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnSetCellLineColor1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
    FTable:Array of Array of TCell;
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
  QFCellProperReturn:=true;
end;

procedure TQFCellProper.Button1Click(Sender: TObject);
var tmp,tmp1,err:integer;
  i,j:integer;
  RowCount,ColCount:integer;
begin
  val(RowEdit.Text,RowCount,err);
  val(ColEdit.Text,ColCount,err);
  val(RowHeightEdit.Text,tmp,err);
  val(ColWidthEdit.Text,tmp1,err);
  for i:=0 to RowCount-1 do
  begin
    StringGrid1.RowHeights[i]:=tmp;
    for j:=1 to ColCount do
    begin
      StringGrid1.ColWidths[j-1]:=tmp1;
    end;
  end;
  val(RowEdit.Text,tmp,err);
  StringGrid1.RowCount:=tmp;
  val(ColEdit.Text,tmp,err);
  StringGrid1.ColCount:=tmp;
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
    FTable[Row,Col].Align:=CbxHAlign.ItemIndex+1;

  if (not ChkBoxMerge.Checked) and ((FTable[Row,Col].ColSpan<>0) or (FTable[Row,Col].RowSpan<>0))  then
  begin
    FTable[Row,Col].RowSpan:=0;
    FTable[Row,Col].ColSpan:=0;
    //FTable[Row,Col].Width:= FColWidth;
    //FTable[Row,Col].Height:= FRowHeight;
  end;

  if LbxFontName.ItemIndex>=0 then
    FTable[Row,Col].FontName:=LbxFontName.Items.ValueFromIndex[LbxFontName.ItemIndex];
  if LbxFontSize.itemindex>=0 then
  begin
    val(LbxFontSize.Items.ValueFromIndex[LbxFontSize.itemindex],tmp,err);
    FTable[Row,Col].FontSize:=tmp;
  end;
  FTable[Row,Col].FontColor:=PanelFontColor.Color;

  FTable[Row,Col].DrawBottom:=DrawBottomLine.Checked;
  FTable[Row,Col].DrawLeft:=DrawLeftLine.Checked;
  FTable[Row,Col].DrawRight:=DrawRightLine.Checked;
  FTable[Row,Col].DrawTop:=DrawTopLine.Checked;
  FTable[Row,Col].LeftLineStyle:=TFPPenStyle(LeftLineStyle.ItemIndex);
  FTable[Row,Col].RightLineStyle:=TFPPenStyle(RightLineStyle.ItemIndex);
  FTable[Row,Col].BottomLineStyle:=TFPPenStyle(BottomLineStyle.ItemIndex);
  FTable[Row,Col].TopLineStyle:=TFPPenStyle(TopLineStyle.ItemIndex);
  FTable[Row,Col].str:=StringGrid1.cells[Row,Col];
end;

procedure TQFCellProper.StringGrid1Click(Sender: TObject);
begin
  row:=StringGrid1.Row;
  col:=StringGrid1.Col+1;
  ComboBox1.ItemIndex:=
     ComboBox1.Items.IndexOf(FTable[Row,Col].ComponentName);
  if FTable[Row,Col].Align=0 then FTable[Row,Col].Align:=2;
  if FTable[Row,Col].Align>0 then
    CbxHAlign.ItemIndex:=FTable[Row,Col].Align-1;

  if FTable[Row,Col].DispType<3 then
    CbxCellType.ItemIndex:=FTable[Row,Col].DispType
  else
    CbxCellType.ItemIndex:=2;//控件
  if (FTable[Row,Col].ColSpan<>0) or (FTable[Row,Col].RowSpan<>0) then
    ChkBoxMerge.Checked:=true
  else
    ChkBoxMerge.Checked:=false;
  EditFontSize.Text:=FTable[Row,Col].FontSize.ToString;
  LbxFontName.ItemIndex:=LbxFontName.Items.IndexOf(FTable[Row,Col].FontName);
  LbxFontSize.ItemIndex:=LbxFontSize.Items.IndexOf(FTable[Row,Col].FontSize.ToString);
  PanelFontColor.Color:=FTable[Row,Col].FontColor;
  DrawBottomLine.Checked:=FTable[Row,Col].DrawBottom;
  DrawLeftLine.Checked:=FTable[Row,Col].DrawLeft;
  DrawRightLine.Checked:=FTable[Row,Col].DrawRight;
  DrawTopLine.Checked:=FTable[Row,Col].DrawTop;
  LeftLineStyle.ItemIndex:=ord(FTable[Row,Col].LeftLineStyle);
  RightLineStyle.ItemIndex:=ord(FTable[Row,Col].RightLineStyle);
  BottomLineStyle.ItemIndex:=ord(FTable[Row,Col].BottomLineStyle);
  TopLineStyle.ItemIndex:=ord(FTable[Row,Col].TopLineStyle);
  PanelFontPreview1.Font.Name:=FTable[Row,Col].FontName;
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
