{*******************************************************}
{                                                       }
{                自定义的富文本控件                     }
{                    TQFRichView                        }
{                  TQFScrollingText                     }
{                                                       }
{          支持Lazarus及windows、linux等平台            }
{           x86_64/aarch64/lonngarch64等CPU             }
{                                                       }
{               Copyright(c) 2024-2024                  }
{              秋风(QQ315795176)原创出品                }
{                                                       }
{                 All rights reserved                   }
{                     保留所有权利                      }
{                                                       }
{*******************************************************}
{
QFComponent for lazarus 包含 4个控件：

1、TQFRichView：采用自定义的富文本格式，类RichView控件，支持超链接、书签跳转等丰富的功能，
   适合作为使用说明等用途；
2、TQFScrollingText：采用自定义的富文本格式，可实现图文的滚动；
3、TQFHorizontalScrollingText：单行横向滚动控件，同样支持自定义富文本格式。
4、TQFGridPanelComponent：秋风原创控件。支持在单元格绑定可视控件，运行时单元格绑定的控件
   吸附到相应的单元格里。重点解决了lazarus跨平台时界面布局问题（用lazarus编写过跨平台应用
   的就清楚，同一代码linux和windows控件位置是不一致的）。
说明：
1、控件的自定义富文件格式渲染等核心功能是秋风独立原创编写。
2、QFRichEditor参考了https://github.com/DreamVB编写的Markdown Editor界面及代码
3、滚动控件的滚动部分参考了lazarus about的思路。
*******************************************************************************}

unit QFComponent;

interface

uses
  Classes, SysUtils, Forms, Controls,  Graphics, ExtCtrls,Dialogs, Printers,
  OSPrinters,StdCtrls,DBCtrls, Menus,QFCellProper, DB,Grids,DBGrids,  FPCanvas,
  fpjson,lazutf8,PublicUnit,LazIDEIntf,EditBtn,DBExtCtrls,
  lclintf, LazFileUtils,  LMessages,StrUtils,QFRichEdit,
  //添加IDE design时鼠标右按弹出菜单需要用到的单元
  ComponentEditors,PropEdits
  ;

const
  ReservedSpace = 1024;

  VerInfo = 'TQFGridPanelComponent';
  Version ='1.0.0.0';

type

  TLineDispType = (ltText,ltLine,lt2Line,ltImg,ltUrl,ltComponent,ltBookMark1,ltBookMark2);

  TLineType = record
     str:string;
     FontName:string[30];
     FontColor:TColor;
     FontStyle:integer;
     FontSize:integer;
     LineHeight:integer;
     Align:integer;
     URL:string;
     URLStr:string;
     DispType:TLineDispType;
     BookMark1:string;
     BookMark2:string;
  end;

  TTableSL = record
     hs1:integer;//表格开始行
     hs2:integer;//表格结束行
     row:integer;//表格的行数
     col:integer;//表格的列数
     RowHeight:integer;//表格行高
  end;

  THyperLink = record
     URL:string;
     URLStr:string;
     Color:TColor;
     x1:integer;
     x2:integer;
     y1:integer;
     y2:integer;
     hs:integer;
  end;

  TQFBookMark = record
     BookMark:string;
     Color:TColor;
     hs:integer;
     x1:integer;
     x2:integer;
     y1:integer;
     y2:integer;
  end;

  // 弹出菜单的标识
  TStMenuItemTag = (mtCopy, mtPaste, mtClearCells, mtSetCellProp);

  TCustomText = class(TCustomControl)//TScrollingWinControl)//TCustomControl)
  private
    FRect:TRect;
    FMV:integer;
    FLineSpacing:integer;//行距
    FTextHeigth:integer;
    FisLeftButtonDown: Boolean; //鼠标左键按下标识
    TTHNO:integer;
    FTS:integer;//表格数量
    FBackgroundImage:TImage;
    FBackImageFile:string;
    FShowBackImage:boolean;
    FIsShowBackImage:boolean;
    FBookMark1:array of TQFBookMark;
    FBookMark2:array of TQFBookMark;
    FHyperLink:array of THyperLink;
    FTablesl:array of TTableSL;
    FTable:Array of Array of TCell;
    FCopyTable:Tcell;
    FOldFontSize:integer;
    FLineList:array of TLineType;
    FActive: boolean;
    FActiveLine: integer; //URL
    FBMActiveLine:integer;//书签
    FBMActiveStr:string;
    FBuffer: TBitmap;
    IMG: TImage;
    FLines: TStrings;
    FOffset: integer;
    FStepSize: integer;
    Lineno:integer;
    FGapX:integer;
    FGapY:integer;
    FColor:TColor;
    FDefaultFontName:string;
    FPathConfig:string;
    function Init(Buffer:TBitmap):boolean;
    function ActiveLineIsURL: boolean;
    function TextPreprocessing(i:integer;str:string;out textstyle:string):string; //文字类预处理
    procedure TablePreprocessing; //表格类预处理
    procedure BackgroundRefresh(Buffer:TBitmap);
    procedure DrawScrollingText(Sender: TObject);
    procedure SetColor(const AValue: TColor);
    function FindMark1(str:string;out NewStr:string):Boolean;
    function FindMark2(str:string;out NewStr:string):Boolean;
    function GetStringTextWidth(Buffer: TBitmap;str:string):integer;
    procedure DisplayChar(Buffer: TBitmap;x,y:integer;str:string); //显示字符
    function DrawTable(Buffer: TBitmap;Index, y:integer):integer;//virtual;
    function TruncationStr(Buffer: TBitmap; str:string;fbwidth:integer):string;//字符串长度超过规定时截断
    function SkipIdentification(i:integer;str:string;out j:integer):string;//跳过标识
    function ReplaceCharacters(str:string):string; //删除所有特殊符号
    procedure GetTableInfo(no:integer);
    procedure GetCellInfo(s:string;out CellType:TCell);
    procedure DrawTexts(Buffer: TBitmap;y:integer);
    procedure SetBackImageFile(AValue: String);
    procedure SetShowBackImage(AValue: Boolean);
    procedure DeleteRecord(index:integer);
  protected
    procedure SetLines(const AValue: TStrings); virtual;
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenFile(files:string);
    procedure RichEditor;
    //procedure RichEditor(const AValue: TQFRichEditor);
 published
    property Align;
    property Anchors;
    property LineSpacing: integer read FLineSpacing write FLineSpacing;
    property Lines: TStrings read FLines write SetLines;
    property GapX: integer read FGapX write FGapX;
    property GapY: integer read FGapY write FGapY;
    property Color: TColor read FColor write SetColor;
    property BackImageFile: string read FBackImageFile write SetBackImageFile;
    property ShowBackImage: Boolean read FShowBackImage write SetShowBackImage;
    //property RichEdit:TQFRichEditor read FQFRE write RichEditor;
  end;

  TQFScrollingText = class(TCustomText)
  private
    FTimer: TTimer;
    procedure DoTimer(Sender: TObject);
    procedure SetActive(const AValue: boolean);
    procedure SetLines(const AValue: TStrings);
    procedure DrawScrollingText(Sender: TObject);
  protected
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: boolean read FActive write SetActive;
  end;

  //横向滚动
  TQFHorizontalScrollingText= class(TCustomText)
  private
    FTimer: TTimer;
    FOffsetX:integer;
    FTextWidth:integer;
    FScrollingText:string;
    procedure HDrawTexts(Buffer: TBitmap;x,y:integer);
    procedure DoTimer(Sender: TObject);
    procedure SetActive(const AValue: boolean);
    procedure SetLines(const AValue: TStrings);
    procedure DrawScrollingText(Sender: TObject);
    procedure SetScrollingText(const AValue: string);
    procedure SetShowBackImage(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ScrollingText:string read FScrollingText write SetScrollingText;
    property Active: boolean read FActive write SetActive;
    property ShowBackImage: Boolean read FShowBackImage write SetShowBackImage;
  end;

  TQFRichView =  class(TCustomText)
  private
    FShowUrlBookMakeHint: Boolean;
    FinitialY: Integer; // 用于存储鼠标按下时的初始位置
    procedure DrawScrollingText(Sender: TObject);
    procedure SetShowUrlBookMakeHint(Value : Boolean);
  protected
    procedure SetLines(const AValue: TStrings);
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SavePicture(Files:string);
    procedure Print;
    procedure PageHeader; //跳到页头
    procedure PageFooter; //跳到页尾
  published
    property StepSize: integer read FStepSize write FStepSize;
    property ShowUrlBookMakeHint: Boolean read FShowUrlBookMakeHint write SetShowUrlBookMakeHint;
  end;

  TQFGridPanelComponent =  class(TCustomText)
  private
    FOldSelectRow:integer;
    FOldSelectCol:integer;
    FSelectRow:integer;
    FSelectCol:integer;
    FMVLineX:integer;
    FMVLineY:integer;
    FControlsList:TStringlist;
    FColCount:integer;
    FRowCount:integer;
    FColSizing:Boolean;
    FRowSizing:Boolean;
    FCellLineColor:TColor;
    FCellLineStyle:TFPPenStyle;
    FMouseDownXY:Tpoint;//用于存储鼠标按下时的初始位置
    FMoveRow:integer;
    FMoveCol:integer;
    FColWidth:integer;
    FRowHeight:integer;
    FTableWidth:integer;
    FTableHeight:integer;
    FRun:integer;
    FGap:integer;
    FBorder:Boolean;
    FOldR:TRect;
    FCurrentR:TRect;
    FResultCursor:TCursor;
    FPOpupMenu:TPopupMenu;
    FEditFontFocusColor:TColor;
    FEditFocusColor:TColor;
    FOldBrushColor:TColor;
    FOldEditFontFocusColor:TColor;
    FOldEditFocusColor:TColor;
    FOldFontName:string;
    FConfigFileName:string;
    procedure TableMerge;
    function Tableiniti:boolean;
    procedure DisplayTable(Sender: TObject);
    procedure DrawTable;
    procedure DrawRect(rect:TRect;colors:TColor;Linewidth,x,y:integer;RepaintRect:Boolean=false);
    procedure SetCellLineColor(Value : TColor);
    procedure SetEditFocusColor(Value : TColor);
    procedure SetEditFontFocusColor(Value : TColor);
    procedure SetCellLineStyle(Value : TFPPenStyle);
    procedure SetColCount(Value :integer);
    procedure SetRowCount(Value :integer);
    function FindChildControls(str:string):TControl;
    procedure MenuItemClick(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure InitPopupMenu;
    procedure LoadJSON(jsonstr:string);
    procedure SaveJSON(files:string);
    function isCell(x,y:integer;out Rect:TRect):Boolean;
    function isComponent(Control:TControl):Boolean;
    procedure ClearCells;
    procedure CopyCells;
    procedure PasteCells;
  protected
    procedure SetLines(const AValue: TStrings);override;
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetCellProper;//设置单元格参数
    procedure Refresh;
    procedure GetControlsList;
    procedure SaveQFConfig(filename:string);
    function LoadQFConfig(filename:string):boolean;
    procedure SaveQFConfig;
    function LoadQFConfig:boolean;
  published
    property RowHeight:integer read FRowHeight write FRowHeight;
    property Gap:integer read FGap write FGap;
    property Border:Boolean read FBorder write FBorder;
    property EditFontFocusColor:TColor read FEditFontFocusColor write SetEditFontFocusColor default clBlack;
    property EditFocusColor:TColor read FEditFocusColor write SetEditFocusColor default clWhite;
    property CellLineColor:TColor read FCellLineColor write SetCellLineColor default clSilver;
    property CellLineStyle:TFPPenStyle read FCellLineStyle write SetCellLineStyle default psSolid;
    property Font;
    property ColCount:integer read FColCount write FColCount;// SetColCount default 5;
    property RowCount:integer read FRowCount write FRowCount;//SetRowCount default 5;
    property ColSizing:Boolean read FColSizing write FColSizing default true;
    property RowSizing:Boolean read FRowSizing write FRowSizing default true;
    property ConfigFileName:string read FConfigFileName write FConfigFileName;
  end;

  //添加IDE design时控件右键设置功能
  TQFGridPanelComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

{$R *.res}

procedure Register;
begin
  //RegisterClasses([TQFRichEditor]);
  //RegisterComponentEditor(TString,TQFRichEditor);
  RegisterComponents('QF Component', [TQFRichView,TQFScrollingText,TQFHorizontalScrollingText,TQFGridPanelComponent]);
end;

{ TScrollingText }

procedure TCustomText.DeleteRecord(index:integer);
var
  FTable1:Array of Array of TCell;
  i,j,i1: Integer;
begin
  FTable1:=nil;
  setlength(FTable1,FTablesl[index].row-1,FTablesl[0].col);
  i1:=0;
  for i := 0 to FTablesl[index].row-1 do
  begin
    if i<>1 then
    begin
    for j:=0 to FTablesl[index].col-1 do
      FTable1[i1,j] := FTable[i ,j]; // 将后面的记录向前移动
      inc(i1);
    end;
  end;
  FTable:=nil;
  setlength(FTable,FTablesl[index].row-1,FTablesl[0].col);
  for i := 0 to FTablesl[index].row-1 - 1 do
  begin
    for j:=0 to FTablesl[index].col-1 do
      FTable[i,j] := FTable1[i ,j]; // 将后面的记录向前移动
  end;
  FTable1:=nil;
end;

procedure TCustomText.SetShowBackImage(AValue: Boolean);
begin
  if (AValue <> FShowBackImage) then
  begin
    FShowBackImage:=AValue;
    Init(FBuffer);
    FOffset:=0;
    DrawTexts(FBuffer,FOffset);
    Canvas.Draw(0,0,FBuffer)
  end;
end;

procedure TCustomText.SetBackImageFile(AValue: String);
begin
  if (AValue <> FBackImageFile) then
  begin
    FBackImageFile:=AValue;
  end;
end;

procedure TCustomText.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    FLines.Assign(AValue);
    Init(FBuffer);
  end;
end;

procedure TCustomText.SetColor(const AValue: TColor);
begin
  FColor:=AValue;
  Init(FBuffer);
  DrawTexts(FBuffer,0);
  Canvas.Draw(0,0,FBuffer)
end;

procedure TCustomText.GetCellInfo(s:string;out CellType:TCell);
var s1:string;
  hlkstr:string;
  url:string;
  urldisptext:string;
  hlk,i,j,tmp,e:integer;
  FontPos1,FontPos2:integer;
  tmp1,tmp2:string;
begin
  CellType.bookmarkstr:='';
  CellType.DispType:=dtText;
  CellType.FontStyle:=cfsNone;
  CellType.FontName:='';
  CellType.FontSize:=0;
  CellType.Color:=clBlack;
  CellType.ColMerge:=0;
  CellType.RowMerge:=0;
  CellType.str:=s;
  CellType.Visible:=true;
  CellType.URL:='';
  CellType.ComponentName:='';
  if pos('[L]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[L]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.Align :=calLeft;
  end;
  if pos('[C]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[C]','',[rfReplaceAll,rfIgnoreCase]);
    CellType.str:=s;
    CellType.Align :=calClient;
  end;
  if pos('[R]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[R]','',[rfReplaceAll,rfIgnoreCase]);
    CellType.str:=s;
    CellType.Align :=calRight;
  end;
  if pos('[#]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[#]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=cfsBold;
  end;
  if pos('[@]',s)>0 then
  begin
    s:=s.Replace('[@]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=cfsStrikeOut;
  end;
  if pos('[$]',s)>0 then
  begin
    s:=s.Replace('[$]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=cfsItalic;
  end;
  if pos('[!]',s)>0 then
  begin
    s:=s.Replace('[!]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=cfsUnderline;
  end;
  if pos('[C1]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[C1]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.Color := clBlack;
  end;
  if pos('[C2]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[C2]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.Color := clRed;
  end;
  if pos('[C3]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[C3]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.Color := clYellow;
  end;
  if pos('[C4]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[C4]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.Color := clGreen;
  end;
  if pos('[C5]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[C5]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.DispType:=dtText;
    CellType.Color := clBlue;
  end;
  if pos('[IMG]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[IMG]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.DispType:=dtPict;
  end;
  if (utf8Pos('<FONTSIZE=', s.ToUpper)>0) then
  begin
    s1:=s;
    FontPos1:=utf8pos('<FONTSIZE=',s1.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s1) do
    begin
      if utf8copy(s1,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s1,FontPos1+10,FontPos2-FontPos1-10);
        val(tmp1,tmp,e);
        CellType.FontSize:=tmp;

        tmp1:=utf8copy(s1,FontPos1,FontPos2-FontPos1+1);
        s1:=s1.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<FONTSIZE',s1.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    s:=s1;
    CellType.str:=s;
    CellType.DispType:=dtText;
  end;
  if (utf8Pos('</FONTSIZE>', s.ToUpper)>0) then
  begin
    s1:=s;
   // CellType.FontSize:=0;
    s1:=s1.Replace('</FONTSIZE>','',[rfReplaceAll, rfIgnoreCase]);
    CellType.str:=s1;
    CellType.DispType:=dtText;
  end;
  if (utf8Pos('<COMPNAME=', s.ToUpper)>0) then
  begin
    s1:=s;
    FontPos1:=utf8pos('<COMPNAME=',s1.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s1) do
    begin
      if utf8copy(s1,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s1,FontPos1+10,FontPos2-FontPos1-10);
        CellType.ComponentName:=tmp1;

        tmp1:=utf8copy(s1,FontPos1,FontPos2-FontPos1+1);
        s1:=s1.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<COMPNAME',s1.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.str:='';//s1;
    CellType.DispType:=dtComponent;//控件类
  end;
  if pos('<BM',s.ToUpper)>0 then
  begin
    s1:=s;
    FontPos1:=utf8pos('<BM',s1.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s1) do
    begin
      if utf8copy(s1,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s1,FontPos1+1,FontPos2-FontPos1-1);
        CellType.bookmarkstr:=tmp1;

        tmp1:=utf8copy(s1,FontPos1,FontPos2-FontPos1+1);
        s1:=s1.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<BM',s1.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.str:=s1;
    CellType.DispType:=dtBookmak1;
  end;
  if pos('[BM',s.ToUpper)>0 then
  begin
    s1:=s;
    FontPos1:=utf8pos('[BM',s1.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s1) do
    begin
      if utf8copy(s1,i,1)=']' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s1,FontPos1+1,FontPos2-FontPos1-1);
        CellType.bookmarkstr:=tmp1;

        tmp1:=utf8copy(s1,FontPos1,FontPos2-FontPos1+1);
        s1:=s1.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('[BM',s1.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.str:=s1;
    CellType.DispType:=dtBookmak2;
  end;
  if utf8pos('<COLMERGE=',s.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<COLMERGE=',s.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s) do
    begin
      if utf8copy(s,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s,FontPos1+10,FontPos2-FontPos1-10);
        val(tmp1,tmp,e);

        tmp1:=utf8copy(s,FontPos1,FontPos2-FontPos1+1);
        s:=s.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<COLMERGE=',s.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.ColMerge:=tmp;
    CellType.str:=s;
  end;
  if utf8pos('<ROWMERGE=',s.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<ROWMERGE=',s.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s) do
    begin
      if utf8copy(s,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s,FontPos1+10,FontPos2-FontPos1-10);
        val(tmp1,tmp,e);

        tmp1:=utf8copy(s,FontPos1,FontPos2-FontPos1+1);
        s:=s.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<ROWMERGE=',s.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.RowMerge:=tmp;
    CellType.str:=s;
  end;
  if utf8pos('<FONTNAME=',s.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<FONTNAME=',s.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s) do
    begin
      if utf8copy(s,i,1)='>' then
      begin
        FontPos2:=i;
        tmp2:=utf8copy(s,FontPos1+10,FontPos2-(FontPos1+10));
        tmp1:=utf8copy(s,FontPos1,FontPos2-FontPos1+1);
        s:=s.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<FONTNAME=',s.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.FontName:=tmp2;
    CellType.str:=s;
  end;
  if pos('</FONTNAME>',s.ToUpper)>0 then
  begin
    s:=s.Replace('</FONTNAME>','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
  end;
  if (pos('<HLK>',s.ToUpper)>0) and (pos('</HLK>',s.ToUpper)>0) then
  begin
    hlkstr:=s;
    hlk:=pos('<HLK>',s.ToUpper)+5;
    hlkstr:=copy(s,hlk,pos('</HLK>',s.ToUpper)-hlk);
    if utf8copy(hlkstr,1,1)='(' then
    begin
      url:='';
      i:=utf8pos(')',hlkstr);
      urldisptext:=utf8copy(hlkstr,2,i-2);
      url:=utf8copy(hlkstr,i+1,utf8length(hlkstr));
    end;
    CellType.url:=url;
    CellType.str:=urldisptext;
    CellType.DispType:=dtLink;
  end
  else
  if (pos('HTTP://',s.ToUpper)>0) or (pos('HTTPS://',s.ToUpper)>0) then
  begin
    CellType.str:=s;
    CellType.DispType:=dtLink;
  end;
end;

function TCustomText.ReplaceCharacters(str:string):string; //删除所有特殊符号
var
  i,j,hlk:integer;
  FontPos1,FontPos2:integer;
  hlkstr,tmp1,tmp2,tmp3:string;
begin
  Result:=str;
  Result:=Result.Replace('<#>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<$>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<!>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<@>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<C1>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<C2>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<C3>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<C4>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<C5>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</C>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<HLK>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</HLK>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</FONTNAME>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</FONTSIZE>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<SUB>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUB>','',[rfReplaceAll, rfIgnoreCase]);
  if utf8pos('<FONTNAME=',Result.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<FONTNAME=',Result.ToUpper);
    i:=FontPos1;
    while i<= utf8length(Result) do
    begin
      if utf8copy(Result,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(Result,FontPos1,FontPos2-FontPos1+1);
        Result:=Result.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<FONTNAME=',Result.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
  end;
  if utf8pos('<FONTSIZE=',Result.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<FONTSIZE=',Result.ToUpper);
    i:=FontPos1;
    while i<= utf8length(Result) do
    begin
      if utf8copy(Result,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(Result,FontPos1,FontPos2-FontPos1+1);
        Result:=Result.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<FONTSIZE=',Result.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
  end;
  if utf8pos('<COLMERGE=',Result.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<COLMERGE=',Result.ToUpper);
    i:=FontPos1;
    while i<= utf8length(Result) do
    begin
      if utf8copy(Result,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(Result,FontPos1,FontPos2-FontPos1+1);
        Result:=Result.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<COLMERGE=',Result.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
  end;
  if utf8pos('<ROWMERGE=',Result.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<ROWMERGE=',Result.ToUpper);
    i:=FontPos1;
    while i<= utf8length(Result) do
    begin
      if utf8copy(Result,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(Result,FontPos1,FontPos2-FontPos1+1);
        Result:=Result.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<ROWMERGE=',Result.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
  end;
  if Assigned(FBookMark1) then
  begin
    if pos('<BM',Result.ToUpper)>0 then
    begin
      for i:=0 to High(FBookMark1) do
      begin
        Result:=Result.Replace('<BM'+(i+1).ToString+'>','',[rfReplaceAll, rfIgnoreCase]);
       end;
    end;
  end;
  if Assigned(FBookMark2) then
  begin
    if pos('[BM',Result.ToUpper)>0 then
    begin
      for i:=0 to High(FBookMark2) do
      begin
        Result:=Result.Replace('[BM'+(i+1).ToString+']','',[rfReplaceAll, rfIgnoreCase]);
      end;
    end;
  end;
end;

function TCustomText.FindMark1(str:string;out NewStr:string):Boolean;
var i:integer;
begin
  Result:=false;
  if pos('<BM',str.ToUpper)>0 then
  begin
    for i:=0 to High(FBookMark1) do
    begin
      if pos('<BM'+(i+1).ToString+'>',str.ToUpper)>0 then
      begin
        NewStr:=str.Replace('<BM'+(i+1).ToString+'>','',[rfReplaceAll, rfIgnoreCase]);
        Result:=true;
        Break;
      end;
    end;
  end;
end;

function TCustomText.FindMark2(str:string;out NewStr:string):Boolean;
var i:integer;
begin
  Result:=false;
  if pos('[BM',str.ToUpper)>0 then
  begin
    for i:=0 to High(FBookMark2) do
    begin
      if pos('[BM'+(i+1).ToString+']',str.ToUpper)>0 then
      begin
        NewStr:=str.Replace('[BM'+(i+1).ToString+']','',[rfReplaceAll, rfIgnoreCase]);
        Result:=true;
        Break;
      end;
    end;
  end;
end;

function TCustomText.GetStringTextWidth(Buffer: TBitmap;str:string):integer;
var
  x,i,j:integer;
  s1,newstr:string;
  oldFontSize,NewFontSize:integer;
  oldStyles:TFontStyles;
  FontPos1,FontPos2:integer;
begin
  x:=0;
  if (pos('<C1>',str.ToUpper)>0) or
  (pos('<C2>',str.ToUpper)>0) or
  (pos('<C3>',str.ToUpper)>0) or
  (pos('<C4>',str.ToUpper)>0) or
  (pos('<C5>',str.ToUpper)>0) or
  (pos('<HLK>',str.ToUpper)>0) or
  (pos('</HLK>',str.ToUpper)>0) or
  (pos('<SUP>',str.ToUpper)>0) or
  (pos('<SUB>',str.ToUpper)>0) or
  (pos('</SUP>',str.ToUpper)>0) or
  (pos('</SUB>',str.ToUpper)>0) or
  (pos('<FONTNAME=',str.ToUpper)>0) or
  (pos('</FONTNAME>',str.ToUpper)>0) or
  (pos('<FONTSIZE=',str.ToUpper)>0) or
  (pos('</FONTSIZE>',str.ToUpper)>0) or
  (pos('<BM',str.ToUpper)>0) or
  (pos('[BM',str.ToUpper)>0) or
  (pos('<COLMERGE=',str.ToUpper)>0) or
  (pos('<ROWMERGE=',str.ToUpper)>0) or
  (pos('</C>',str.ToUpper)>0) or
  (pos('<$>',str)>0) or
  (pos('<!>',str)>0) or
  (pos('<@>',str)>0) or
  (pos('<#>',str)>0) or
  (pos('</>',str)>0)
  then
  begin
    i:=0;
    oldStyles:=Buffer.Canvas.font.Style;
    oldFontSize:=Buffer.Canvas.font.Size;
    NewFontSize:=Buffer.Canvas.font.Size div 2;
    if NewFontSize=0 then NewFontSize:=5;
    while i<=utf8length(str) do
    begin
      s1:=utf8copy(str,i,1);
       //<BMx>书签1
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='B')
         and (utf8copy(str,i+2,1).ToUpper='M') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //[BMx]书签2
      if (s1='[') and (utf8copy(str,i+1,1).ToUpper='B')
         and (utf8copy(str,i+2,1).ToUpper='M') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)=']' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //ColMerge
      if (s1='<')
         and (utf8copy(str,i+1,1).ToUpper='C')
         and (utf8copy(str,i+2,1).ToUpper='O')
         and (utf8copy(str,i+3,1).ToUpper='L')
         and (utf8copy(str,i+4,1).ToUpper='M')
         and (utf8copy(str,i+5,1).ToUpper='E')
         and (utf8copy(str,i+6,1).ToUpper='R')
         and (utf8copy(str,i+7,1).ToUpper='G')
         and (utf8copy(str,i+8,1).ToUpper='E')
         and (utf8copy(str,i+9,1).ToUpper='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //RowMerge
      if (s1='<')
         and (utf8copy(str,i+1,1).ToUpper='R')
         and (utf8copy(str,i+2,1).ToUpper='O')
         and (utf8copy(str,i+3,1).ToUpper='w')
         and (utf8copy(str,i+3,1).ToUpper='M')
         and (utf8copy(str,i+5,1).ToUpper='E')
         and (utf8copy(str,i+6,1).ToUpper='R')
         and (utf8copy(str,i+7,1).ToUpper='G')
         and (utf8copy(str,i+8,1).ToUpper='E')
         and (utf8copy(str,i+9,1).ToUpper='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //<FontName=xx>
      if (s1='<')
         and (utf8copy(str,i+1,1).ToUpper='F')
         and (utf8copy(str,i+2,1).ToUpper='O')
         and (utf8copy(str,i+3,1).ToUpper='N')
         and (utf8copy(str,i+4,1).ToUpper='T')
         and (utf8copy(str,i+5,1).ToUpper='N')
         and (utf8copy(str,i+6,1).ToUpper='A')
         and (utf8copy(str,i+7,1).ToUpper='M')
         and (utf8copy(str,i+8,1).ToUpper='E')
         and (utf8copy(str,i+9,1).ToUpper='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //</FontName>
      if (s1='<')
         and (utf8copy(str,i+1,1).ToUpper='/')
         and (utf8copy(str,i+2,1).ToUpper='F')
         and (utf8copy(str,i+3,1).ToUpper='O')
         and (utf8copy(str,i+4,1).ToUpper='N')
         and (utf8copy(str,i+5,1).ToUpper='T')
         and (utf8copy(str,i+6,1).ToUpper='N')
         and (utf8copy(str,i+7,1).ToUpper='A')
         and (utf8copy(str,i+8,1).ToUpper='M')
         and (utf8copy(str,i+9,1).ToUpper='E')
         and (utf8copy(str,i+10,1).ToUpper='>') then
      begin
        i:=i+11;
      end
      else
      //<Fontsize=xx>
      if (s1='<')
         and (utf8copy(str,i+1,1).ToUpper='F')
         and (utf8copy(str,i+2,1).ToUpper='O')
         and (utf8copy(str,i+3,1).ToUpper='N')
         and (utf8copy(str,i+4,1).ToUpper='T')
         and (utf8copy(str,i+5,1).ToUpper='S')
         and (utf8copy(str,i+6,1).ToUpper='I')
         and (utf8copy(str,i+7,1).ToUpper='Z')
         and (utf8copy(str,i+8,1).ToUpper='E')
         and (utf8copy(str,i+9,1).ToUpper='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //</Fontsize>
      if (s1='<')
         and (utf8copy(str,i+1,1).ToUpper='/')
         and (utf8copy(str,i+2,1).ToUpper='F')
         and (utf8copy(str,i+3,1).ToUpper='O')
         and (utf8copy(str,i+4,1).ToUpper='N')
         and (utf8copy(str,i+5,1).ToUpper='T')
         and (utf8copy(str,i+6,1).ToUpper='S')
         and (utf8copy(str,i+7,1).ToUpper='I')
         and (utf8copy(str,i+8,1).ToUpper='Z')
         and (utf8copy(str,i+9,1).ToUpper='E')
         and (utf8copy(str,i+10,1).ToUpper='>') then
      begin
        i:=i+11;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='H') and (utf8copy(str,i+2,1).ToUpper='L')
         and (utf8copy(str,i+3,1).ToUpper='K') and (utf8copy(str,i+4,1)='>') then
      begin
        i:=i+5;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='/') and (utf8copy(str,i+2,1).ToUpper='H')
         and (utf8copy(str,i+3,1).ToUpper='L') and (utf8copy(str,i+4,1)='K') and (utf8copy(str,i+5,1)='>') then
      begin
        i:=i+6;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='S') and (utf8copy(str,i+2,1).ToUpper='U')
         and (utf8copy(str,i+3,1).ToUpper='P') and (utf8copy(str,i+4,1)='>') then
      begin
        Buffer.Canvas.font.Size:=NewFontSize;//上标
        i:=i+5;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='S') and (utf8copy(str,i+2,1).ToUpper='U')
         and (utf8copy(str,i+3,1).ToUpper='B') and (utf8copy(str,i+4,1)='>') then
      begin
        Buffer.Canvas.font.Size:=NewFontSize;//下标
        i:=i+5;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='/')  and (utf8copy(str,i+2,1).ToUpper='S')
         and (utf8copy(str,i+3,1).ToUpper='U')
         and (utf8copy(str,i+4,1).ToUpper='P') and (utf8copy(str,i+5,1)='>') then
      begin
        Buffer.Canvas.font.Size:=oldFontSize;//取消上标
        i:=i+6;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='/')  and (utf8copy(str,i+2,1).ToUpper='S')
         and (utf8copy(str,i+3,1).ToUpper='U')
         and (utf8copy(str,i+4,1).ToUpper='B') and (utf8copy(str,i+5,1)='>') then
      begin
        Buffer.Canvas.font.Size:=oldFontSize;//取消下标
        i:=i+6;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='!') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsUnderline];//下划线
        i:=i+3;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='$') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsItalic];//斜体
        i:=i+3;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='@') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsStrikeOut];//删除线
        i:=i+3;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='#') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsBold];//加粗
        i:=i+3;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='/') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=oldStyles;//恢复原风格
        i:=i+3;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='C') then
      begin
        if (utf8copy(str,i+2,1)='1') and (utf8copy(str,i+3,1)='>') then i:=i+4;
        if (utf8copy(str,i+2,1)='2') and (utf8copy(str,i+3,1)='>') then i:=i+4;
        if (utf8copy(str,i+2,1)='3') and (utf8copy(str,i+3,1)='>') then i:=i+4;
        if (utf8copy(str,i+2,1)='4') and (utf8copy(str,i+3,1)='>') then i:=i+4;
        if (utf8copy(str,i+2,1)='5') and (utf8copy(str,i+3,1)='>') then i:=i+4;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='/') and (utf8copy(str,i+2,1).ToUpper='C')
         and (utf8copy(str,i+3,1)='>') then
      begin
        i:=i+4;
      end
      else
      begin
        x:=x+Buffer.Canvas.TextWidth(s1);
        inc(i);
      end;
    end;
    Result:=x;
  end
  else
    Result:=Buffer.Canvas.TextWidth(str);
end;

procedure TCustomText.BackgroundRefresh(Buffer:TBitmap);
begin
  FRect.Top:=0;
  FRect.Left:=0;
  FRect.Width:=Width;
  FRect.Height:=Height;
  if (trim(FBackImageFile)<>'') and (FShowBackImage) then
  begin
    if  FileExists(FPathConfig+FBackImageFile) then
    begin
      if FBackgroundImage=nil then
      begin
        FBackgroundImage:=TImage.Create(self);
        FBackgroundImage.Picture.LoadFromFile(FPathConfig+FBackImageFile);
      end;
      Buffer.Canvas.StretchDraw(FRect,FBackgroundImage.Picture.Bitmap);
      FIsShowBackImage:=true;
    end
    else
    begin
      FIsShowBackImage:=false;
      with Buffer.Canvas do
      begin
        Brush.Color := FColor;
        Brush.Style := bsSolid;
        FillRect(FRect);
      end;
    end;
  end
  else
  begin
    with Buffer.Canvas do
    begin
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      FillRect(FRect);
    end;
  end;
end;

function TCustomText.TextPreprocessing(i:integer;str:string;out textstyle:string):string;
var newbmstr,nbms:string;
  hlkstr:string;
  tmp1,tmp2,tmp3:string;
  j,pos1,pos2,hlk:integer;
begin
  Result:=str;
  textstyle:='';
  FLineList[i].DispType:=ltText;
  FLineList[i].URLStr:='';
  FLineList[i].URL:='';
  FLineList[i].FontSize:=FOldFontSize;
  FLineList[i].Align :=1;
  FLineList[i].FontStyle:=0;
  //行对齐模式
  if pos('[L]',str.ToUpper)>0 then
  begin
   textstyle:=textstyle+'[L]';
   str:=str.Replace('[L]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
   FLineList[i].Align :=1;
  end;
  if pos('[C]',str.ToUpper)>0 then
  begin
   textstyle:=textstyle+'[C]';
   str:=str.Replace('[C]','',[rfReplaceAll,rfIgnoreCase]);
   FLineList[i].Align :=2;
  end;
  if pos('[R]',str.ToUpper)>0 then
  begin
   textstyle:=textstyle+'[R]';
   str:=str.Replace('[R]','',[rfReplaceAll,rfIgnoreCase]);
   FLineList[i].Align :=3;
  end;
  //字体风格
  if pos('[#]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[#]';
    str:=str.Replace('[#]','',[rfReplaceAll,rfIgnoreCase]);
    FLineList[i].FontStyle :=1;// fsBold;
  end;
  if pos('[@]',str)>0 then
  begin
    textstyle:=textstyle+'[@]';
    str:=str.Replace('[@]','',[rfReplaceAll,rfIgnoreCase]);
    FLineList[i].FontStyle :=2;// fsStrikeOut;
  end;
  if pos('[$]',str)>0 then
  begin
    textstyle:=textstyle+'[$]';
    str:=str.Replace('[$]','',[rfReplaceAll,rfIgnoreCase]);
    FLineList[i].FontStyle :=3;// fsItalic;
  end;
  if pos('[!]',str)>0 then
  begin
    textstyle:=textstyle+'[!]';
    str:=str.Replace('[!]','',[rfReplaceAll,rfIgnoreCase]);
    FLineList[i].FontStyle :=4;// fsUnderline;
  end;
  //字体颜色
  if pos('[C1]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[C1]';
     str:=str.Replace('[C1]','',[rfReplaceAll,rfIgnoreCase]);
     FLineList[i].FontColor := clBlack;
  end;
  if pos('[C2]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[C2]';
     str:=str.Replace('[C2]','',[rfReplaceAll,rfIgnoreCase]);
     FLineList[i].FontColor := clRed;
  end;
  if pos('[C3]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[C3]';
     str:=str.Replace('[C3]','',[rfReplaceAll,rfIgnoreCase]);
     FLineList[i].FontColor := clYellow;
  end;
  if pos('[C4]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[C4]';
     str:=str.Replace('[C4]','',[rfReplaceAll,rfIgnoreCase]);
     FLineList[i].FontColor := clGreen;
  end;
  if pos('[C5]',str.ToUpper)>0 then
  begin
     textstyle:=textstyle+'[C5]';
     str:=str.Replace('[C5]','',[rfReplaceAll,rfIgnoreCase]);
     FLineList[i].FontColor := clBlue;
  end;
  //字体尺寸
  if pos('[S1]',str.ToUpper)>0 then
  begin
    str:=str.Replace('[S1]','',[rfReplaceAll,rfIgnoreCase]);
    textstyle:=textstyle+'[S1]';
    FLineList[i].FontSize := 9
  end;
  if pos('[S2]',str.ToUpper)>0 then
  begin
    str:=str.Replace('[S2]','',[rfReplaceAll,rfIgnoreCase]);
    textstyle:=textstyle+'[S2]';
    FLineList[i].FontSize := 12
  end;
  if pos('[S3]',str.ToUpper)>0 then
  begin
    str:=str.Replace('[S3]','',[rfReplaceAll,rfIgnoreCase]);
    textstyle:=textstyle+'[S3]';
    FLineList[i].FontSize := 14
  end;
  if pos('[S4]',str.ToUpper)>0 then
  begin
    str:=str.Replace('[S4]','',[rfReplaceAll,rfIgnoreCase]);
    textstyle:=textstyle+'[S4]';
    FLineList[i].FontSize := 16
  end;
  if pos('[S5]',str.ToUpper)>0 then
  begin
    str:=str.Replace('[S5]','',[rfReplaceAll,rfIgnoreCase]);
    textstyle:=textstyle+'[S5]';
    FLineList[i].FontSize := 18;
  end;
  //分割线
  if (pos('[LINE]',str.ToUpper)>0) or (pos('***',str.ToUpper)>0) or
     (pos('___',str.ToUpper)>0) or (pos('---',str.ToUpper)>0) then
  begin
    textstyle:=textstyle+'[LINE]';
    FLineList[i].DispType:=ltLine;
    str:=str.Replace('[LING]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
  end;
  //双分割线
  if pos('[2LINE]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[2LINE]';
    FLineList[i].DispType:=lt2Line;
    str:=str.Replace('[2LING]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
  end;
  //图像
  if pos('[IMG]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[IMG]';
    FLineList[i].DispType:=ltImg;
    str:=str.Replace('[IMG]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    if  FileExists(FPathConfig+str) then
    begin
      IMG.Picture.LoadFromFile(FPathConfig+str);
    end;
  end;
  //超链接
  if (Pos('HTTP://', str.ToUpper) >= 1) or (Pos('HTTPS://', str.ToUpper) >= 1) then
  begin
    hlkstr:=str;
    if utf8pos('<HLK>',str.ToUpper)>0 then
    begin
      hlk:=utf8pos('<HLK>',str.ToUpper)+5;
      hlkstr:=utf8copy(str,hlk,utf8pos('</HLK>',str.ToUpper)-hlk);
      tmp1:='';
      tmp2:='';
      tmp3:='';
      if (utf8pos('(',hlkstr)>0) and (utf8pos(')',hlkstr)>0) then
      begin
        hlkstr:=utf8copy(hlkstr,utf8pos(')',hlkstr)+1,utf8length(hlkstr));
        FLineList[i].URLStr:=utf8copy(str,utf8pos('(',str)+1,utf8pos(')',str)-utf8pos('(',str)-1);
        tmp1:=utf8copy(str,1,utf8pos('<HLK>',str.ToUpper)-1);
        tmp2:=FLineList[i].URLStr;
        tmp3:=utf8copy(str,utf8pos('</HLK>',str.ToUpper)+6,utf8length(str));
        str:=tmp1+'<HLK>'+tmp2+'</HLK>'+tmp3;
      end;
    end
    else
    begin
      str:='<HLK>'+str+'</HLK>';
    end;

    FLineList[i].FontColor := clBlue;
    FLineList[i].url:=hlkstr;
    FLineList[i].DispType:=ltUrl;
  end
  else
  if (utf8Pos('<COMPNAME=', str.ToUpper)>0) then
  begin
    pos1:=utf8pos('<COMPNAME=',str.ToUpper);
    for j:= pos1 to utf8length(str) do
    begin
      if utf8copy(str,j,1)='>' then
      begin
        pos2:=j;
        break;
      end;
    end;
    nbms:=utf8copy(str,pos1+1,pos2-pos1-1);
    textstyle:=textstyle+'<COMPNAME='+nbms+'>';
    str:=str.Replace('<COMPNAME='+nbms+'>','',[rfReplaceAll,rfIgnoreCase]);
    FLineList[i].DispType:=ltComponent;
  end
  else
  if (utf8Pos('<BM', str.ToUpper)>0) then
  begin
    FindMark1(str,newbmstr);
    pos1:=utf8pos('<BM',str.ToUpper);
    for j:= pos1 to utf8length(str) do
    begin
      if utf8copy(str,j,1)='>' then
      begin
        pos2:=j;
        break;
      end;
    end;
    nbms:=utf8copy(str,pos1+1,pos2-pos1-1);
    textstyle:=textstyle+'<'+nbms+'>';
    str:=newbmstr;
    FLineList[i].BookMark1:=nbms;
    FLineList[i].DispType:=ltBookMark1;
    FLineList[i].FontColor := clBlue;
  end;
  if (Pos('[BM', str.ToUpper)>0) then
  begin
    FindMark2(str,newbmstr);
    pos1:=utf8pos('[BM',str.ToUpper);
    for j:= pos1 to utf8length(str) do
    begin
      if utf8copy(str,j,1)=']' then
      begin
        pos2:=j;
        break;
      end;
    end;
    nbms:=utf8copy(str,pos1+1,pos2-pos1-1);
    textstyle:=textstyle+'['+nbms+']';
    str:=newbmstr;
    FLineList[i].BookMark2:=nbms;
    FLineList[i].DispType:=ltBookMark2;
  end;
  Result:=str;
end;

procedure TCustomText.TablePreprocessing;
var
  i,j,row,col,js,gw:integer;
  no,k,hl:integer;
  str:string;
  FBMSL1:integer;//书签数量
  FBMSL2:integer;
  rh:integer;//行高
  i1,i2,e:integer;
  strsub:string;
begin
  //解析是否包含表格
  //解析有几个表格
  FTS:=0;
  hl:=0;
  FBMSL1:=0; //书签数量
  FBMSL2:=0; //书签数量
  for i := 0 to FLines.Count-1 do
  begin
    str := Trim(FLines[i]);
    if (pos('[BM',str.ToUpper)>0) and (pos(']',str.ToUpper)>0) then
    begin
      inc(FBMSL2);//书签数量
    end
    else
    if (pos('<BM',str.ToUpper)>0) and (pos('>',str.ToUpper)>0) then
    begin
      inc(FBMSL1);//书签数量
    end
    else
    if (pos('HTTP',str.ToUpper)>0) then //URL数量
    begin
       inc(hl);
    end
    else
    if pos('|',str)>0 then
    begin
      if (pos('|',Trim(FLines[i+1]))=0) or
         (
         (pos('|',Trim(FLines[i+1]))>0) and
         (pos('|',Trim(FLines[i+2]))>0) and
         (pos('-',Trim(FLines[i+2]))>0)
         ) then //表格最后一行
      begin
        inc(FTS);
      end;
    end;
  end;
  if hl>0 then
  begin
    if Assigned(FHyperLink) then
      FHyperLink:=nil;
    setlength(FHyperLink,hl);
  end;
  if FBMSL1>0 then
  begin
    if Assigned(FBookMark1) then
      FBookMark1:=nil;
    setlength(FBookMark1,FBMSL1);
  end;
  if FBMSL2>0 then
  begin
    if Assigned(FBookMark2) then
      FBookMark2:=nil;
    setlength(FBookMark2,FBMSL2);
  end;
  setlength(FTablesl,FTS);//FTS--表格数量
  ////////////////////////////////
  col:=0;
  row:=0;
  js:=0;
  no:=0;
  hl:=0;
  for i := 0 to FLines.Count-1 do
  begin
    str := Trim(FLines[i]);
    i1:=utf8pos('<ROWH=',str.ToUpper);//读取表格行高
    if i1>0 then
    begin
      i2:=utf8pos('>',str.ToUpper);
      strsub:=utf8copy(str,i1+6,i2-i1-6);
      val(strsub,rh,e);
    end;
    if pos('|',str)>0 then
    begin
      for j:=0 to utf8length(str) do
      begin
        if utf8copy(str,j,1)='|' then  //表格
        begin
          if js=0 then
          begin
            inc(row);
            js:=1;
          end;
          if row=1 then
          begin
            inc(col);
            FTablesl[no].hs1:=i;//开始行数
          end;
        end;
      end;
      if (pos('|',Trim(FLines[i+1]))=0) or
         (
         (pos('|',Trim(FLines[i+1]))>0) and
         (pos('|',Trim(FLines[i+2]))>0) and
         (pos('-',Trim(FLines[i+2]))>0)
         ) then //表格最后一行
      begin
        FTablesl[no].row:=row;//行数
        FTablesl[no].col:=col;//列数
        FTablesl[no].hs2:=i;//结束行数
        FTablesl[no].RowHeight:=rh;
        inc(no);
        rh:=0;
        row:=0;
        col:=0;
        js:=0;
      end;
    end;
    js:=0;
  end;
end;

function TCustomText.Init(Buffer:TBitmap):boolean;
var
  i,w,j,k,rj,hls:integer;
  bmsl1,bmsl2:integer;
  s,s1,textstyle:string;
  Linetemp: TStringList;
begin
  Lineno:=0;
  Linetemp:=TStringList.Create;
  setlength(FLineList,FLines.Count);
  k:= FLines.Count;
  Result:=false;
  if k>0 then
  begin
    Result:=true;
    TablePreprocessing;//表格预处理

    //根据控件宽度进行自动换行处理
    hls:=0;
    for i := 0 to FLines.Count-1 do
    begin
      s := FLines[i];
      textstyle:='';
      rj:=0;
      if Length(s) > 0 then
      begin
        s:=TextPreprocessing(i,s,textstyle);
        Buffer.Canvas.Font.Size:=FLineList[i].FontSize;
        w:=Buffer.Canvas.TextWidth(ReplaceCharacters(s));
        if FLineList[i].DispType=ltUrl then
        begin
          FHyperLink[hls].URL:= FLineList[i].url;
          FHyperLink[hls].URLStr:= FLineList[i].URLStr;
          //FHyperLink[hls].hs:=i;//所在行数
          inc(hls);
        end;
        if w>Width then //换行
        begin
           s1:='';
           k:=0;
           j:=1;
           while j<= utf8length(s) do
           begin
             s1:=s1+utf8copy(s,j,1);
             if Buffer.Canvas.TextWidth(ReplaceCharacters(s1)+SkipIdentification(j,s,rj))+FGapX*2>=Width then //跳过特定符号
             begin
               if SkipIdentification(j,s,rj)=''then
                 s1:=s1+utf8copy(s,j+1,rj);
               Linetemp.Add(textstyle+s1);
               s1:='';
             end;
             if SkipIdentification(j,s,rj)='' then
             begin
               s1:=s1+utf8copy(s,j+1,rj);
               j:=j+rj+1;
             end
             else
               inc(j);
           end;
           if s1<>'' then
             Linetemp.Add(textstyle+s1);
        end
        else
          Linetemp.Add(textstyle+s);
      end;
    end;
    //根据控件宽度进行自动换行处理

    FLineList:=nil;
    setlength(FLineList,Linetemp.Count);
    Lineno:= Linetemp.Count;
    hls:=0;
    bmsl1:=0;//BM
    bmsl2:=0;
    for i := 0 to Linetemp.Count-1 do
    begin
      s := Linetemp[i];
      if Length(s) > 0 then
      begin
        s:=TextPreprocessing(i,s,textstyle);
        FLineList[i].str:=s;
        Buffer.Canvas.Font.Size:=FLineList[i].FontSize;
        FLineList[i].LineHeight:=Buffer.Canvas.TextHeight(s)+FLineSpacing;

        //if FLineList[i].DispType='URL' then
        if utf8pos('<HLK>',FLineList[i].str.ToUpper)>0 then
        begin
          //FHyperLink[hls].URL:= FLineList[i].url;
          FHyperLink[hls].hs:=i;//所在行数
          FLineList[i].URL:=FHyperLink[hls].URL;
          inc(hls);
        end
        else
        if FLineList[i].DispType=ltBookMark1 then
        begin
           FBookMark1[bmsl1].BookMark:=FLineList[i].BookMark1;
           FBookMark1[bmsl1].hs:=i;//所在行数
           inc(bmsl1);
        end
        else
        if FLineList[i].DispType=ltBookMark2 then
        begin
          FBookMark2[bmsl2].BookMark:=FLineList[i].BookMark2;
          FBookMark2[bmsl2].hs:=i;//所在行数
          inc(bmsl2);
        end;
      end;
    end;
    freeandnil(Linetemp);
  end;
  Buffer.Width := Width;
  Buffer.Height := Height;
  if FOffset = -1 then
    FOffset := Buffer.Height;

  BackgroundRefresh(Buffer); //刷新背景
end;

procedure TCustomText.DrawScrollingText(Sender: TObject);
begin
  //BackgroundRefresh(FBuffer); //刷新背景
  //FOffset:=0;
  //DrawTexts(FBuffer,FOffset);
  //Canvas.Draw(0,0,FBuffer)
end;

procedure TCustomText.GetTableInfo(no:integer);
var col,row,row1,js,i,j,dc:integer;
  s,str:string;
  MyCell:TCell;
begin
  //解释表格有几行几列
{
|姓名|单位|地址|电话|
|:-:|:-|-:|:-:|
|秋风1|检测中心1|南山建工村1|183233640|
|秋风2|检测中心2|南山建工村2|283233640|
|秋风3|检测中心3|南山建工村3|383233640|
}
  FTable:=nil;
  setlength(FTable,FTablesl[no].row,FTablesl[no].col);
  js:=0;
  col:=0;
  row:=0;
  row1:=0;
  for i := FTablesl[no].hs1 to FTablesl[no].hs2 do
  begin
    s := Trim(FLines[i]);
    if pos('|',s)>=0 then
    begin
      str:='';
      col:=0;
      for j:=0 to utf8length(s) do
      begin
        dc:=0;
        if utf8copy(s,j,1)='|' then  //表格
        begin
          if col<=(FTablesl[no].col-1) then
          begin
            if row=1 then  //第2行为表格单元的对齐方式
            begin
              if str=':-' then
              FTable[0,col].Align:=calLeft//居左
              else
              if str=':-:' then
              FTable[0,col].Align:=calClient//居中
              else
              if str='-:' then
               FTable[0,col].Align:=calRight//居右
            end
            else
            begin
              if row<=FTablesl[no].row-1 then
              begin
                GetCellInfo(str,MyCell);
                if row>1 then row1:=row-1
                else row1:=row;
                FTable[row1,col].str:= MyCell.str;
                FTable[row1,col].URL:= MyCell.URL;
                FTable[row1,col].DispType:= MyCell.DispType;
                FTable[row1,col].bookmarkstr:= MyCell.bookmarkstr;
                FTable[row1,col].Align:=MyCell.Align;
                FTable[row1,col].Color:=MyCell.Color;
                FTable[row1,col].FontStyle:=MyCell.FontStyle;
                FTable[row1,col].FontName:=MyCell.FontName;
                FTable[row1,col].FontSize:=MyCell.FontSize;
                FTable[row1,col].ColMerge:=MyCell.ColMerge;
                FTable[row1,col].RowMerge:=MyCell.RowMerge;
                FTable[row1,col].Visible:=MyCell.Visible;
                FTable[row1,col].ComponentName:=MyCell.ComponentName;
              end;
            end;
          end;
          inc(col);
          dc:=j;
          str:='';
        end
        else
        begin
          str:=str+utf8copy(s,j,1);
        end;
      end;
    end;
    inc(row);
    js:=0;
  end;
end;

//跳过标识(表格文字)
function TCustomText.SkipIdentification(i:integer;str:string;out j:integer):string;
var k:integer;
  FontPos1,FontPos2:integer;
begin
  Result:=utf8copy(str,i,1);
  j:=0;
  if (Result='<') and
    (utf8copy(str,i+1,1).ToUpper='/') and
    (utf8copy(str,i+2,1).ToUpper='F') and
    (utf8copy(str,i+3,1).ToUpper='O') and
    (utf8copy(str,i+4,1).ToUpper='N') and
    (utf8copy(str,i+5,1).ToUpper='T') and
    (utf8copy(str,i+6,1).ToUpper='N') and
    (utf8copy(str,i+7,1).ToUpper='A') and
    (utf8copy(str,i+8,1).ToUpper='M') and
    (utf8copy(str,i+9,1).ToUpper='E') and
    (utf8copy(str,i+10,1).ToUpper='>') then
  begin
    Result:='';
    j:=11;
  end
  else
  if (Result='<') and
    (utf8copy(str,i+1,1).ToUpper='F') and
    (utf8copy(str,i+2,1).ToUpper='O') and
    (utf8copy(str,i+3,1).ToUpper='N') and
    (utf8copy(str,i+4,1).ToUpper='T') and
    (utf8copy(str,i+5,1).ToUpper='N') and
    (utf8copy(str,i+6,1).ToUpper='A') and
    (utf8copy(str,i+7,1).ToUpper='M') and
    (utf8copy(str,i+8,1).ToUpper='E') and
    (utf8copy(str,i+9,1).ToUpper='=') then
  begin
    FontPos1:=i;
    for k:=i to utf8length(str) do
    begin
      if utf8copy(str,k,1)='>' then
      begin
        FontPos2:=k;
        break;
      end;
    end;
    Result:='';
    j:=(FontPos2-FontPos1)+1;
  end
  else
  //ColMerge=    MERGE
  if (Result='<') and
    (utf8copy(str,i+1,1).ToUpper='C') and
    (utf8copy(str,i+2,1).ToUpper='O') and
    (utf8copy(str,i+3,1).ToUpper='L') and
    (utf8copy(str,i+4,1).ToUpper='M') and
    (utf8copy(str,i+5,1).ToUpper='E') and
    (utf8copy(str,i+6,1).ToUpper='R') and
    (utf8copy(str,i+7,1).ToUpper='G') and
    (utf8copy(str,i+8,1).ToUpper='E') and
    (utf8copy(str,i+9,1).ToUpper='=') then
  begin
    FontPos1:=i;
    for k:=i to utf8length(str) do
    begin
      if utf8copy(str,k,1)='>' then
      begin
        FontPos2:=k;
        break;
      end;
    end;
    Result:='';
    j:=(FontPos2-FontPos1)+1;
  end
  else
  //RowMerge=
  if (Result='<') and
    (utf8copy(str,i+1,1).ToUpper='R') and
    (utf8copy(str,i+2,1).ToUpper='O') and
    (utf8copy(str,i+3,1).ToUpper='W') and
    (utf8copy(str,i+4,1).ToUpper='M') and
    (utf8copy(str,i+5,1).ToUpper='E') and
    (utf8copy(str,i+6,1).ToUpper='R') and
    (utf8copy(str,i+7,1).ToUpper='G') and
    (utf8copy(str,i+8,1).ToUpper='E') and
    (utf8copy(str,i+9,1).ToUpper='=') then
  begin
    FontPos1:=i;
    for k:=i to utf8length(str) do
    begin
      if utf8copy(str,k,1)='>' then
      begin
        FontPos2:=k;
        break;
      end;
    end;
    Result:='';
    j:=(FontPos2-FontPos1)+1;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='F') and
    (utf8copy(str,i+2,1).ToUpper='O') and
    (utf8copy(str,i+3,1).ToUpper='N') and
    (utf8copy(str,i+4,1).ToUpper='T') and
    (utf8copy(str,i+5,1).ToUpper='S') and
    (utf8copy(str,i+6,1).ToUpper='I') and
    (utf8copy(str,i+7,1).ToUpper='Z') and
    (utf8copy(str,i+8,1).ToUpper='E') and
    (utf8copy(str,i+9,1).ToUpper='=') then
  begin
    FontPos1:=i;
    for k:=i to utf8length(str) do
    begin
      if utf8copy(str,k,1)='>' then
      begin
        FontPos2:=k;
        break;
      end;
    end;
    Result:='';
    j:=(FontPos2-FontPos1)+1;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='/') and
    (utf8copy(str,i+2,1).ToUpper='F') and
    (utf8copy(str,i+3,1).ToUpper='O') and
    (utf8copy(str,i+4,1).ToUpper='N') and
    (utf8copy(str,i+5,1).ToUpper='T') and
    (utf8copy(str,i+5,1).ToUpper='S') and
    (utf8copy(str,i+6,1).ToUpper='I') and
    (utf8copy(str,i+7,1).ToUpper='Z') and
    (utf8copy(str,i+8,1).ToUpper='E') and
    (utf8copy(str,i+9,1).ToUpper='>') then
  begin
    Result:='';
    j:=10;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='B') and
     (utf8copy(str,i+2,1).ToUpper='M') then
  begin
    for k:=i+2 to length(str) do
    begin
     if utf8copy(str,k,1)='>' then
     begin
       Result:='';
       j:=k-i+1;
       break;
     end;
    end;
  end
  else
  if (Result='[') and (utf8copy(str,i+1,1).ToUpper='B') and
     (utf8copy(str,i+2,1).ToUpper='M') then
  begin
    for k:=i+2 to length(str) do
    begin
     if utf8copy(str,k,1)=']' then
     begin
       Result:='';
       j:=k-i+1;
       break;
     end;
    end;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='H') and   //HLK超链接
     (utf8copy(str,i+2,1).ToUpper='L') and
     (utf8copy(str,i+3,1).ToUpper='K') and
     (utf8copy(str,i+4,1)='>') then
  begin
    Result:='';
    j:=5;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='/') and
     (utf8copy(str,i+2,1).ToUpper='H') and
     (utf8copy(str,i+3,1).ToUpper='L') and
     (utf8copy(str,i+4,1).ToUpper='K') and
     (utf8copy(str,i+5,1)='>') then
  begin
    Result:='';
    j:=6;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='S') and
     (utf8copy(str,i+2,1).ToUpper='U') and
     (utf8copy(str,i+3,1).ToUpper='P') and
     (utf8copy(str,i+4,1)='>') then
  begin
    Result:='';
    j:=5;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='S') and
     (utf8copy(str,i+2,1).ToUpper='U') and
     (utf8copy(str,i+3,1).ToUpper='B') and
     (utf8copy(str,i+4,1)='>') then
  begin
    Result:='';
    j:=5;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='/') and
     (utf8copy(str,i+2,1).ToUpper='S') and
     (utf8copy(str,i+3,1).ToUpper='U') and
     (utf8copy(str,i+4,1).ToUpper='P') and
     (utf8copy(str,i+5,1)='>') then
  begin
    Result:='';
    j:=6;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='/') and
     (utf8copy(str,i+2,1).ToUpper='S') and
     (utf8copy(str,i+3,1).ToUpper='U') and
     (utf8copy(str,i+4,1).ToUpper='B') and
     (utf8copy(str,i+5,1)='>') then
  begin
    Result:='';
    j:=6;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1)='#') and (utf8copy(str,i+2,1)='>') then
  begin
    Result:='';
    j:=3;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1)='!') and (utf8copy(str,i+2,1)='>') then
  begin
    Result:='';
    j:=3;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1)='$') and (utf8copy(str,i+2,1)='>') then
  begin
    Result:='';
    j:=3;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1)='@') and (utf8copy(str,i+2,1)='>') then
  begin
    Result:='';
    j:=3;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1)='/') and (utf8copy(str,i+2,1)='>') then
  begin
    Result:='';
    j:=3;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='C') then
  begin
    if (utf8copy(str,i+2,1)='1') and (utf8copy(str,i+3,1)='>') then
    begin
       Result:='';
       j:=3;
    end;
    if (utf8copy(str,i+2,1)='2') and (utf8copy(str,i+3,1)='>') then
    begin
       Result:='';
       j:=3;
    end;
    if (utf8copy(str,i+2,1)='3') and (utf8copy(str,i+3,1)='>') then
    begin
       Result:='';
       j:=3;
    end;
    if (utf8copy(str,i+2,1)='4') and (utf8copy(str,i+3,1)='>') then
    begin
       Result:='';
       j:=3;
    end;
    if (utf8copy(str,i+2,1)='5') and (utf8copy(str,i+3,1)='>') then
    begin
       Result:='';
       j:=3;
    end;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1)='/') and (utf8copy(str,i+2,1).ToUpper='C') and (utf8copy(str,i+3,1)='>') then
  begin
    Result:='';
    j:=4;
  end;
end;

procedure TCustomText.DisplayChar(Buffer: TBitmap;x,y:integer;str:string);
var i,j,s,e,fs:integer;
  DStr:string;
  zwh,ywh,k:integer;
  oldFontSize,NewFontSize:integer;
  oldColor:TColor;
  oldStyles:TFontStyles;
  oldy,supy,suby:integer;
  NewFontName,tmpstr:string;
  FontPos1,FontPos2:integer;
  FH1,FH2:integer;
begin
  if (pos('<C1>',str.ToUpper)>0) or
  (pos('<C2>',str.ToUpper)>0) or
  (pos('<C3>',str.ToUpper)>0) or
  (pos('<C4>',str.ToUpper)>0) or
  (pos('<C5>',str.ToUpper)>0) or
  (pos('<HLK>',str.ToUpper)>0) or
  (pos('</HLK>',str.ToUpper)>0) or
  (pos('<SUP>',str.ToUpper)>0) or
  (pos('<SUB>',str.ToUpper)>0) or
  (pos('</SUP>',str.ToUpper)>0) or
  (pos('</SUB>',str.ToUpper)>0) or
  (pos('</C>',str.ToUpper)>0) or
  (pos('<!>',str)>0) or
  (pos('<$>',str)>0) or
  (pos('<@>',str)>0) or
  (pos('<#>',str)>0) or
  (pos('</FONTNAME>',str.ToUpper)>0) or
  (pos('<FONTNAME=',str.ToUpper)>0) or
  (pos('<FONTSIZE=',str.ToUpper)>0) or
  (pos('</FONTSIZE>',str.ToUpper)>0) or
  (pos('<COLMERGE=',str.ToUpper)>0) or
  (pos('<ROWMERGE=',str.ToUpper)>0) or
  (FindMark1(str,DStr)) or
  (FindMark2(str,DStr)) or
  (pos('</>',str)>0)
  then
  begin
    i:=1;
    oldColor:=Buffer.Canvas.font.Color;
    oldStyles:=Buffer.Canvas.font.Style;
    oldFontSize:=Buffer.Canvas.font.Size;
    oldy:=y;
    supy:=y;
    suby:=y+(Buffer.Canvas.TextHeight('国') div 2)-5;
    NewFontSize:=Buffer.Canvas.font.Size div 2;
    Buffer.Canvas.Brush.Style := bsClear;//透明文字
    if NewFontSize=0 then NewFontSize:=5;
    FH1:=0;
    FH2:=0;
    while i<=utf8length(str) do
    begin
      DStr:=utf8copy(str,i,1);
      //<FontName=xx>
      if (DStr='<')
         and (utf8copy(str,i+1,1).ToUpper='F')
         and (utf8copy(str,i+2,1).ToUpper='O')
         and (utf8copy(str,i+3,1).ToUpper='N')
         and (utf8copy(str,i+4,1).ToUpper='T')
         and (utf8copy(str,i+5,1).ToUpper='N')
         and (utf8copy(str,i+6,1).ToUpper='A')
         and (utf8copy(str,i+7,1).ToUpper='M')
         and (utf8copy(str,i+8,1).ToUpper='E')
         and (utf8copy(str,i+9,1)='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
        NewFontName:=utf8copy(str,FontPos1+10,FontPos2-FontPos1-10);
        FH1:=Buffer.Canvas.GetTextHeight(NewFontName);
        Buffer.Canvas.font.Name:=NewFontName;//设为新字体
        FH2:=Buffer.Canvas.GetTextHeight(NewFontName);
        y:=y+abs(FH2-FH1) div 2;//修正字体不同时的高度差
      end
      else
      if (DStr='<')
         and (utf8copy(str,i+1,1).ToUpper='/')
         and (utf8copy(str,i+2,1).ToUpper='F')
         and (utf8copy(str,i+3,1).ToUpper='O')
         and (utf8copy(str,i+4,1).ToUpper='N')
         and (utf8copy(str,i+5,1).ToUpper='T')
         and (utf8copy(str,i+6,1).ToUpper='N')
         and (utf8copy(str,i+7,1).ToUpper='A')
         and (utf8copy(str,i+8,1).ToUpper='M')
         and (utf8copy(str,i+9,1).ToUpper='E')
         and (utf8copy(str,i+10,1)='>') then
      begin
        i:=i+11;
        Buffer.Canvas.font.Name:=FDefaultFontName;//恢复默认字体
        y:=y-abs(FH2-FH1) div 2;//恢复原来的显示位置
      end
      else
      if (DStr.ToUpper='C')
        and (utf8copy(str,i+1,1).ToUpper='O')
        and (utf8copy(str,i+2,1).ToUpper='L')
        and (utf8copy(str,i+3,1).ToUpper='M')
        and (utf8copy(str,i+4,1).ToUpper='E')
        and (utf8copy(str,i+5,1).ToUpper='R')
        and (utf8copy(str,i+6,1).ToUpper='G')
        and (utf8copy(str,i+7,1).ToUpper='E')
        and (utf8copy(str,i+8,1)='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      if (DStr.ToUpper='R')
        and (utf8copy(str,i+1,1).ToUpper='O')
        and (utf8copy(str,i+2,1).ToUpper='W')
        and (utf8copy(str,i+3,1).ToUpper='M')
        and (utf8copy(str,i+4,1).ToUpper='E')
        and (utf8copy(str,i+5,1).ToUpper='R')
        and (utf8copy(str,i+6,1).ToUpper='G')
        and (utf8copy(str,i+7,1).ToUpper='E')
        and (utf8copy(str,i+8,1)='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //<fontsize=xx>
      if (DStr.ToUpper='<')
        and (utf8copy(str,i+1,1).ToUpper='F')
        and (utf8copy(str,i+2,1).ToUpper='O')
        and (utf8copy(str,i+3,1).ToUpper='N')
        and (utf8copy(str,i+4,1).ToUpper='T')
        and (utf8copy(str,i+5,1).ToUpper='S')
        and (utf8copy(str,i+6,1).ToUpper='I')
        and (utf8copy(str,i+7,1).ToUpper='Z')
        and (utf8copy(str,i+8,1).ToUpper='E')
        and (utf8copy(str,i+9,1)='=') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            tmpstr:=utf8copy(str,FontPos1,FontPos2-FontPos1);
            val(tmpstr,fs,e);
            Break;
          end;
        end;
        tmpstr:=UTF8copy(str,FontPos1+10,FontPos2-(FontPos1+10));
        val(tmpstr,s,e);
        Buffer.Canvas.font.Size:=s;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      //</fontsize>
      if (DStr.ToUpper='<')
        and (utf8copy(str,i+1,1).ToUpper='/')
        and (utf8copy(str,i+2,1).ToUpper='F')
        and (utf8copy(str,i+3,1).ToUpper='O')
        and (utf8copy(str,i+4,1).ToUpper='N')
        and (utf8copy(str,i+5,1).ToUpper='T')
        and (utf8copy(str,i+6,1).ToUpper='S')
        and (utf8copy(str,i+7,1).ToUpper='I')
        and (utf8copy(str,i+8,1).ToUpper='Z')
        and (utf8copy(str,i+9,1).ToUpper='E') then
      begin
        FontPos1:=i;
        for j:=i to  utf8length(str) do
        begin
          if utf8copy(str,j,1)='>' then
          begin
            FontPos2:=j;
            Break;
          end;
        end;
        Buffer.Canvas.font.Size:=oldFontSize;
        //tmpstr:=copy(str,i+9,j-i+9);
        //val(tmpstr,s,e);
        //Buffer.Canvas.font.Size:=s;
        i:=i+(FontPos2-FontPos1)+1;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='H') and (utf8copy(str,i+2,1).ToUpper='L')
        and (utf8copy(str,i+3,1).ToUpper='K') and (utf8copy(str,i+4,1)='>') then
      begin
         i:=i+5;
         Buffer.Canvas.font.Color:=clBlue;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='/') and (utf8copy(str,i+2,1).ToUpper='H')
        and (utf8copy(str,i+3,1).ToUpper='L') and (utf8copy(str,i+4,1).ToUpper='K')
        and (utf8copy(str,i+5,1)='>') then
      begin
         i:=i+6;
         Buffer.Canvas.font.Color:=oldColor;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='B') and //<BMxx>书签目录
         (utf8copy(str,i+2,1).ToUpper='M') then
      begin
        for k:=i+2 to length(str) do
        begin
          if utf8copy(str,k,1)='>' then
          begin
            i:=(k-i)+2;
            break;
          end;
        end;
      end
      else
      if (DStr='[') and (utf8copy(str,i+1,1).ToUpper='B') and//[BMxx]书签
         (utf8copy(str,i+2,1).ToUpper='M') then
      begin
        for k:=i+2 to length(str) do
        begin
          if utf8copy(str,k,1)=']' then
          begin
            i:=(k-i)+2;
            break;
          end;
        end;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='S') and (utf8copy(str,i+2,1).ToUpper='U')
         and (utf8copy(str,i+3,1).ToUpper='P') and (utf8copy(str,i+4,1)='>') then
      begin
        Buffer.Canvas.font.Size:=NewFontSize;//上标
        y:=supy;
        i:=i+5;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='S') and (utf8copy(str,i+2,1).ToUpper='U')
         and (utf8copy(str,i+3,1).ToUpper='B') and (utf8copy(str,i+4,1)='>') then
      begin
        Buffer.Canvas.font.Size:=NewFontSize;//下标
        y:=suby;
        i:=i+5;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='/')  and (utf8copy(str,i+2,1).ToUpper='S')
         and (utf8copy(str,i+3,1).ToUpper='U')
         and (utf8copy(str,i+4,1).ToUpper='P') and (utf8copy(str,i+5,1)='>') then
      begin
        Buffer.Canvas.font.Size:=oldFontSize;//取消上标
        y:=oldy;
        i:=i+6;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='/')  and (utf8copy(str,i+2,1).ToUpper='S')
         and (utf8copy(str,i+3,1).ToUpper='U')
         and (utf8copy(str,i+4,1).ToUpper='B') and (utf8copy(str,i+5,1)='>') then
      begin
        Buffer.Canvas.font.Size:=oldFontSize;//取消下标
        y:=oldy;
        i:=i+6;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1)='$') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsItalic];//斜体
        i:=i+3;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1)='!') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsUnderline];//下划线
        i:=i+3;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1)='@') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsStrikeOut];//删除线
        i:=i+3;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1)='#') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsBold];//加粗
        i:=i+3;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1)='/') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=oldStyles;//恢复原风格
        i:=i+3;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='C') then
      begin
        if (utf8copy(str,i+2,1)='1') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clBlack;
        if (utf8copy(str,i+2,1)='2') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clRed;
        if (utf8copy(str,i+2,1)='3') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clYellow;
        if (utf8copy(str,i+2,1)='4') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clGreen;
        if (utf8copy(str,i+2,1)='5') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clBlue;
        i:=i+4;
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1)='/') and (utf8copy(str,i+2,1).ToUpper='C') and (utf8copy(str,i+3,1)='>') then
      begin
        Buffer.Canvas.font.Color:=oldColor;
        i:=i+4;
      end
      else
      begin
        ywh:=0;
        if DStr<>'' then
        begin
          if ord(DStr[1])<127 then
          begin
            ywh:=Buffer.Canvas.TextHeight(DStr);
            zwh:=Buffer.Canvas.TextHeight('国');
            if ywh<>zwh then ywh:=abs(zwh-ywh) div 2
            else ywh:=0;
          end;
        end;
        Buffer.Canvas.TextOut(x, y+ywh, DStr);//在linux中文和英文显示高度有差别
        x:=x+Buffer.Canvas.TextWidth(DStr);
        inc(i);
      end;
    end;
  end
  else
    Buffer.Canvas.TextOut(x, y, str);
end;

//将超过单元格宽度的字符串截断
function TCustomText.TruncationStr(Buffer: TBitmap; str:string;fbwidth:integer):string;
var w1,i,j:integer;
    tmp,repstr:string;
begin
   Result:=str;
   tmp:='';
   j:=0;
   w1:=Buffer.Canvas.TextWidth(ReplaceCharacters(str))+5;
   if w1>fbwidth then
   begin
      tmp:='';
      i:=1;
      while i<= utf8length(str) do
      begin
        tmp:=tmp+utf8copy(str,i,1);
        repstr:=SkipIdentification(i,str,j);
        if Buffer.Canvas.TextWidth(ReplaceCharacters(tmp)+repstr)+5>fbwidth then //跳过特定符号
        begin
          Result:=tmp;
          Break;
        end;
        if repstr='' then
        begin
          tmp:=tmp+utf8copy(str,i+1,j);
          i:=i+j+1;
        end
        else
          inc(i);
      end;
   end;
end;

function TCustomText.DrawTable(Buffer: TBitmap;Index,y:integer):integer;
var
  i,j,w1,h,h1,row,col,v:integer;
  i1,k:integer;
  colWidth:integer;
  x0,y0,x1,y1:integer;
  TableRowHeight:integer;
  c:integer;
  th:integer;//文字高度
  row1,col1,oo,oo1:integer;
begin
  //if FTablesl<>nil then
  //DeleteRecord(index);//删除第2行定义单元格的对齐格式

  row:=FTablesl[Index].row-2;
  col:=FTablesl[Index].col-1;
  TableRowHeight:=FTablesl[Index].RowHeight;
  Buffer.Canvas.Font.Style:=[fsBold];
  th:= Buffer.Canvas.TextHeight('国');
  h:=round(th*1.5); //表格行高
  if TableRowHeight>h then h:=TableRowHeight;
  colWidth:=(Buffer.Width-FGapX*2) div col; //单元格宽
  Buffer.Canvas.Pen.Color:=clBlack;//黑色画笔
  //重新计算合并后单元格的width和height
  for i:=0 to row do
  begin
    for j:=0 to col do
    begin
      FTable[i,j].Height:=h;
      FTable[i,j].Width:=colWidth;
      //竖向合并单元格
      if (FTable[i,j].RowMerge>0) and (FTable[i,j].ColMerge>0) then
      begin
        row1:=FTable[i,j].RowMerge+i-1;
        if row1>row then row1:=row;

        for oo:=i to row1 do
        begin
          col1:=FTable[i,j].ColMerge+j-1;
          if col1>col then col1:=col;
          for oo1:=j to col1 do
          begin
            FTable[oo,oo1].Visible:=false;
          end;
          FTable[i,j].Visible:=true;//将左上角单元格置为true
          FTable[i,j].Width:=colWidth*FTable[i,j].ColMerge;
          FTable[i,j].Height:=h*FTable[i,j].RowMerge;
        end;
      end
      else
      //竖向合并单元格
      if FTable[i,j].RowMerge>0 then
      begin
        FTable[i,j].Height:=h*FTable[i,j].RowMerge;
        row1:=FTable[i,j].RowMerge+i-1;
        if row1>row then row1:=row;
        for oo:=i+1 to row1 do
        begin
          FTable[oo,j].Visible:=false;
        end;
      end
      else
      //横向合并单元格
      if FTable[i,j].ColMerge>0 then
      begin
        FTable[i,j].Width:=colWidth*FTable[i,j].ColMerge;
        col1:=FTable[i,j].ColMerge+j-1;
        if col1>col then col1:=col;
        for oo:=j+1 to col1 do
        begin
          FTable[i,oo].Visible:=false;
        end;
      end;
    end;
  end;
  //画单元格
  for i:=0 to row do
  begin
    for j:=1 to col do
    begin
      if FTable[i,j].Visible then
      begin
        //顶横线
        x0:=(j-1)*colWidth+FGapX;
        y0:=FOffset+y+FGapY+3+i*h;
        x1:=x0+FTable[i,j].Width;
        y1:=y0;
        Buffer.Canvas.Line(x0,y0,x1,y1);

        //底横线
        x0:=(j-1)*colWidth+FGapX;
        y0:=FOffset+y+FGapY+3+i*h+FTable[i,j].Height;
        x1:=x0+FTable[i,j].Width;//+FGapX;
        y1:=y0;
        Buffer.Canvas.Line(x0,y0,x1,y1);

        //左竖线
        x0:=(j-1)*colWidth+FGapX;
        y0:=FOffset+i*h+y+FGapY+3;
        x1:=x0;
        y1:=y0+FTable[i,j].Height;
        Buffer.Canvas.Line(x0,y0,x1,y1);

        //右竖线
        x0:=x0+FTable[i,j].Width;
        y0:=FOffset+i*h+y+FGapY+3;
        x1:=x0;
        y1:=y0+FTable[i,j].Height;
        if x0>=Buffer.Width then
          x0:=Buffer.Width-1;
        if x1>=Buffer.Width then
          x1:=Buffer.Width-1;
        Buffer.Canvas.Line(x0,y0,x1,y1);
      end;
    end;
  end;

  for i:=0 to row do  //绘表格文字
  begin
    for j:=0 to col-1 do
    begin
      if FTable[i,j+1].Visible then
      begin
        if (FTable[i,j+1].DispType=dtText) or (FTable[i,j+1].DispType=dtBookmak1)
           or (FTable[i,j+1].DispType=dtBookmak2) or (FTable[i,j+1].DispType=dtLink) then  //显示文字
        begin
          //设置字体属性
          if FTable[i,j+1].FontName<>'' then
            Buffer.Canvas.Font.Name:=FTable[i,j+1].FontName;
          if FTable[i,j+1].FontStyle=cfsNone then Buffer.Canvas.Font.Style:=[];
          if FTable[i,j+1].FontStyle=cfsBold then Buffer.Canvas.Font.Style:=[fsBold];
          if FTable[i,j+1].FontStyle=cfsStrikeOut then Buffer.Canvas.Font.Style:=[fsStrikeOut];
          if FTable[i,j+1].FontStyle=cfsItalic then Buffer.Canvas.Font.Style:=[fsItalic];
          if FTable[i,j+1].FontStyle=cfsUnderline then Buffer.Canvas.Font.Style:=[fsUnderline];
          if FTable[i,j+1].DispType>dtPict then  Buffer.Canvas.Font.Color:=clBlue //URL,bookmark
          else Buffer.Canvas.Font.Color:=FTable[i,j+1].Color;
          //设置字体属性

          x0:=FGapX+j*colWidth;
          h1:=h;
          w1:=colWidth;
          if FTable[i,j+1].Width>0 then
            colWidth:=FTable[i,j+1].Width;

          if FTable[i,j+1].Height>0 then
             h:=FTable[i,j+1].Height;

          x1:=x0; //居左
          if FTable[0,j+1].Align=calLeft then
            x1:=x0 ;//居左
         if FTable[0,j+1].Align=calClient then
            x1:=x0+(colWidth-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j+1].str,colWidth))) div 2; //居中
          if FTable[0,j+1].Align=calRight then
             x1:=x0+(colWidth-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j+1].str,colWidth)))-5; //居右
          if i=0 then
          begin
            x1:=x0+(colWidth-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j+1].str,colWidth))) div 2;//标题行文字居中
            y0:=FOffset + y+FGapY+i*h+abs(h- th) div 2;//垂直居中
            Buffer.Canvas.Font.Style:=[fsBold];
            Buffer.Canvas.Font.Color:=FTable[i,j+1].Color;
            DisplayChar(Buffer,x1+2, y0+5,TruncationStr(Buffer,FTable[i,j+1].str,colWidth));//截断超过单元格宽度的字符串
          end
          else
          begin
             if h1<>h then
               y0:=FOffset + y + FGapY + (i)* h1 + abs (h-th) div 2//垂直居中
             else
               y0:=FOffset + y + FGapY + (i)* h1 + abs(h1- th) div 2;//垂直居中
             DisplayChar(Buffer,x1+2, y0+5,TruncationStr(Buffer,FTable[i,j+1].str,colWidth));
          end;
          colWidth:=w1;
          h:=h1;
        end;

        if FTable[i,j+1].DispType=dtLink then //确定URL在表格的位置
        begin
          for k:=0 to high(FHyperLink) do
          begin
            if FHyperLink[k].url=FTable[i,j+1].URL then// .str then
            begin
              FHyperLink[k].x1:=x1;
              FHyperLink[k].x2:=x1+Buffer.Canvas.TextWidth(FTable[i,j+1].str);
              FHyperLink[k].y1:=y+FGapY+(i)*h+abs(h- th) div 2;
              FHyperLink[k].y2:=y+FGapY+(i)*h+(abs(h- th) div 2)+Buffer.Canvas.TextHeight(FLineList[i].str); //超链接出现时的高度;
              Break;
            end;
          end;
        end
        else
        if FTable[i,j+1].DispType=dtBookmak1 then //BOOKMARK1
        begin
          for k:=0 to high(FBookMark1) do
          begin
            if FBookMark1[k].BookMark=FTable[i,j+1].bookmarkstr then
            begin
              FBookMark1[k].x1:=x1;
              FBookMark1[k].x2:=x1+GetStringTextWidth(Buffer,FTable[i,j+1].str);
              FBookMark1[k].y1:=y+FGapY+(i)*h+abs(h- th) div 2;
              FBookMark1[k].y2:=y+FGapY+(i)*h+(abs(h- th) div 2)+Buffer.Canvas.TextHeight(FLineList[i].str); //书签目录的高度;
              Break;
            end;
          end;
        end
        else
        if FTable[i,j+1].DispType=dtBookmak2 then//BOOKMARK2
        begin
          for k:=0 to high(FBookMark2) do
          begin
            if FBookMark2[k].BookMark=FTable[i,j+1].bookmarkstr then
            begin
              FBookMark2[k].y1:= y+FGapY+(i)*h+abs(h- th) div 2;
              FBookMark2[k].y2:= y+FGapY+(i)*h+(abs(h- th) div 2)+Buffer.Canvas.TextHeight(FLineList[i].str); //书签的高度;
              Break;
            end;
          end;
        end
        else
        if FTable[i,j+1].DispType=dtPict then  //显示图形
        begin
          x0:=FGapX+j*colWidth+1;
          y0:=FOffset + y+FGapY+(i)*h+4;
          if  FileExists(FPathConfig+FTable[i,j+1].str) then
          begin
            IMG.Picture.LoadFromFile(FPathConfig+FTable[i,j+1].str);
            //设置图像显示位置及尺寸（单元格大小）
            FRect.Top:=y0;
            FRect.Left:=x0;
            if FTable[i,j+1].Width<>0 then
              FRect.Width:=FTable[i,j+1].Width-1
            else
              FRect.Width:=colWidth-1;
            if FTable[i,j+1].Height<>0 then
              FRect.Height:=FTable[i,j+1].Height-1
            else
              FRect.Height:=h-1;
            Buffer.Canvas.StretchDraw(FRect,img.Picture.Bitmap);
          end
          else
          begin
            //没找到图像文件
            DisplayChar(Buffer,x0+2, y0,TruncationStr(Buffer,'['+ExtractFileName(FTable[i,j+1].str+']'),colWidth));
          end;
        end;
      end;
    end;
    FTable[i,0].Height:=Buffer.Canvas.TextHeight('国')+2;
  end;
  y:=y+(row+1)*h+5;
  Result:=y;
end;

procedure TCustomText.DrawTexts(Buffer: TBitmap;y:integer);
var
  w,x,disptable: integer;
  s: string;
  urlpos1,urlpos2:integer;
  urlposStr,urlstr:string;
  tsno,addh:integer;
  i,k: integer;
begin
  disptable:=0;
  tsNo:=0;
  addh:=1;
  FTextHeigth:=0;
  for i:=0 to  Lineno - 1 do
  begin
    if FLineList[i].DispType=ltLine then
    begin
      Buffer.Canvas.Pen.Color:=FLineList[i].FontColor;
      Buffer.Canvas.Line(FGapX,FOffset + y+FGapY+3,Buffer.Width-FGapX,FOffset + y+FGapY+3);
      y:=y+5;
    end
    else
    if FLineList[i].DispType=lt2Line then
    begin
      Buffer.Canvas.Pen.Color:=FLineList[i].FontColor;
      Buffer.Canvas.Line(FGapX,FOffset + y+FGapY,Buffer.Width-FGapX,FOffset + y+FGapY);
      Buffer.Canvas.Line(FGapX,FOffset + y+FGapY+3,Buffer.Width-FGapX,FOffset + y+FGapY+3);
      y:=y+5;
    end
    else
    if FLineList[i].DispType=ltImg then
    begin
      if  FileExists(FPathConfig+FLineList[i].str) then
      begin
        IMG.Picture.LoadFromFile(FPathConfig+FLineList[i].str);
        x:=FGapX;
        if FLineList[i].Align=1 then //行居左
         x:=FGapX;
        if FLineList[i].Align=2 then //行居中
         x:=FGapX+(Buffer.Width - img.Picture.Width) div 2;
        if FLineList[i].Align=3 then //行居右
         x:=(Buffer.Width - img.Picture.Width)-FGapX;
        Buffer.Canvas.Draw(x, FOffset+y, img.Picture.Bitmap);
        y:=y+img.Picture.Height+5;
      end;
    end
    else
    if pos('|',FLineList[i].str)>0 then//画表格
    begin
      if (i>=FTablesl[TsNo].hs1) and (i<=FTablesl[TsNo].hs2) then
      begin
        GetTableInfo(TsNo);
        y:=DrawTable(Buffer,TsNo,y);
        inc(disptable);
        if tsno<FTS then
        begin
          inc(TsNo);
          if TTHNO=-1 then
            TTHNO:=0;
        end;
      end;
    end
    else
    begin
      Buffer.Canvas.Font.Size:=FLineList[i].FontSize;
      if FLineList[i].FontStyle=0 then
        Buffer.Canvas.Font.Style:=[];
      if FLineList[i].FontStyle=1 then
          Buffer.Canvas.Font.Style:=[fsBold];
      if FLineList[i].FontStyle=2 then
          Buffer.Canvas.Font.Style:=[fsStrikeOut];
      if FLineList[i].FontStyle=3 then
          Buffer.Canvas.Font.Style:=[fsItalic];
      if FLineList[i].FontStyle=4 then
          Buffer.Canvas.Font.Style:=[fsUnderline];
      Buffer.Canvas.Font.Color:=FLineList[i].FontColor;
      w:=GetStringTextWidth(Buffer,FLineList[i].str);
      if FLineList[i].Align=1 then //行居左
        x:=FGapX;
      if FLineList[i].Align=2 then //行居中
        x:=FGapX+(Buffer.Width - w) div 2;
      if FLineList[i].Align=3 then //行居右
        x:=(Buffer.Width - w)-FGapX;

      //在指定的位置显示字符串
      DisplayChar(Buffer,x, FOffset + y+FGapY, FLineList[i].str);

      if FLineList[i].DispType=ltBookMark1 then
      begin
        for k:=0 to high(FBookMark1) do
        begin
          if FBookMark1[k].hs=i then
          begin
            FBookMark1[k].x1:=x;
            FBookMark1[k].x2:=x+Buffer.Canvas.TextWidth(FLineList[i].str);
            FBookMark1[k].y1:=y;
            FBookMark1[k].y2:=y+Buffer.Canvas.TextHeight(FLineList[i].str); //书签目录的高度;
            Break;
          end;
        end;
      end
      else
      if FLineList[i].DispType=ltBookMark2 then
      begin
        for k:=0 to high(FBookMark2) do
        begin
          if FBookMark2[k].hs=i then
          begin
            FBookMark2[k].y1:=y;
            FBookMark2[k].y2:=y+Buffer.Canvas.TextHeight(FLineList[i].str); //书签的高度;
            Break;
          end;
        end;
      end
      else
      if FLineList[i].URL<>'' then
      begin
        for k:=0 to high(FHyperLink) do
        begin
          if FHyperLink[k].URL=FLineList[i].URL then
          begin
            if pos('<HLK>',FLineList[i].str.ToUpper)>0 then
            begin
              urlpos1:=utf8pos('<HLK>',FLineList[i].str.ToUpper);
              urlpos2:=utf8pos('</HLK>',FLineList[i].str.ToUpper);
              urlposStr:=utf8copy(FLineList[i].str,1,urlpos1-1);
              urlstr:=utf8copy(FLineList[i].str,urlpos1+5,urlpos2-urlpos1-5);
              FHyperLink[k].x1:=x+Buffer.Canvas.TextWidth(urlposStr);
              FHyperLink[k].x2:=FHyperLink[k].x1+Buffer.Canvas.TextWidth(urlstr);
            end
            else
            begin
              FHyperLink[k].x1:=x;
              FHyperLink[k].x2:=x+Buffer.Canvas.TextWidth(FLineList[i].str);
            end;
            FHyperLink[k].y1:=y;
            FHyperLink[k].y2:=y+Buffer.Canvas.TextHeight(FLineList[i].str); //超链接出现时的高度;
            Break;
          end;
        end;
      end;
      y:=y+ FLineList[i].LineHeight-FGapY*2+5;
    end;
  end;
  FTextHeigth:=y;
end;

function TCustomText.ActiveLineIsURL: boolean;
begin
  if (FActiveLine > 0) and (FActiveLine < Lineno) then
   //Result := (Pos('http://', FLineList[FActiveLine].str) >= 1) or (Pos('https://', FLineList[FActiveLine].str) >= 1)
    Result := (Pos('http://', FLineList[FActiveLine].URL) >= 1) or (Pos('https://', FLineList[FActiveLine].URL) >= 1)
  else
    Result := False;
end;

procedure TCustomText.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;

  TTHNO:=-1;
  Init(FBuffer);
end;

procedure TCustomText.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if ActiveLineIsURL then
  begin
    OpenURL(FLineList[FActiveLine].URL);
    FisLeftButtonDown := False;
  end;
end;

procedure TCustomText.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

constructor TCustomText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  FLines := TStringList.Create;
  FBuffer := TBitmap.Create;
  img:=TImage.Create(nil);

  Parent:=TWinControl(AOwner);
  FLineSpacing:=0;
  FBackImageFile:='';
  FStepSize := 1;
  TTHNO:=-1;
  FOffset := -1;
  FGapX:=0;
  FGapY:=0;
  FOldFontSize:=FBuffer.Canvas.Font.Size;
  FDefaultFontName:=FBuffer.Canvas.Font.Name;
  FColor:=clWhite;
  FShowBackImage:=true;
  FIsShowBackImage:=false;
  OnPaint := @DrawScrollingText;
  if (csDesigning in ComponentState) or Assigned(LazarusIDE) then
  begin
    FPathConfig:=ExtractFilePath(LazarusIDE.ActiveProject.Files[0].Filename);
  end
  else
    FPathConfig:=GetCurrentDir+DirectorySeparator;
end;

destructor TCustomText.Destroy;
begin
  if Assigned(FTable) then
    FTable:=nil;
  if Assigned(FLineList) then
  FLineList:=nil;
  if Assigned(FHyperLink) then
    FHyperLink:=nil;
  if Assigned(FBackgroundImage) then
    FBackgroundImage.Free;
  if Assigned(FBookMark1) then
    FBookMark1:=nil;
  if Assigned(FBookMark2) then
    FBookMark2:=nil;
  FLines.Free;
  img.Free;
  FBuffer.Free;

  inherited Destroy;
end;

procedure TCustomText.RichEditor;
//procedure TCustomText.RichEditor(const AValue: TQFRichEditor);
var QFRichEdit:TQFRichEditor;
begin
  QFRichEdit:=TQFRichEditor.Create(nil);
  QFRichEdit.RichEdit.Text:=FLines.Text;
  QFRichEdit.ShowModal;
  FLines.Text:=QFRichEdit.RichEdit.Text;

  Init(FBuffer);
  BackgroundRefresh(FBuffer);//刷新背景
  DrawTexts(FBuffer,0);
  Canvas.Draw(0,0,FBuffer);
  QFRichEdit.Free;
end;

procedure TCustomText.OpenFile(files:string);
var
  od: TOpenDialog;
begin
  if trim(files)='' then
  begin
    od := TOpenDialog.Create(nil);
    od.Title := '打开';
    od.Filter := 'QF富文本文件(*.QF)|*.QF|文本文件(*.txt)|*.txt|所有文件(*.*)|*.*';

    if od.Execute then
      FLines.LoadFromFile(od.FileName);
    od.Free;
  end
  else
  begin
    if  FileExists(FPathConfig+files) then
      FLines.LoadFromFile(FPathConfig+files);
  end;
  Init(FBuffer);
  BackgroundRefresh(FBuffer);//刷新背景
  DrawTexts(FBuffer,0);
  Canvas.Draw(0,0,FBuffer);
end;

constructor TQFScrollingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnPaint := @DrawScrollingText;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer:=@DoTimer;
  FTimer.Interval:=30;
  FMV:=0;
end;

destructor TQFScrollingText.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TQFScrollingText.SetActive(const AValue: boolean);
begin
  FMV:=0;
  FActive := AValue;
  if FActive then
  begin
    Init(FBuffer);
  end;
  FTimer.Enabled:=Active;
end;

procedure TQFScrollingText.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    if FActive then
      FTimer.Enabled:=false;
    FLines.Assign(AValue);
    Init(FBuffer);
    if FActive then
      FTimer.Enabled:=FActive;
  end;
end;

procedure TQFScrollingText.DrawScrollingText(Sender: TObject);
begin
  if Active then
    Canvas.Draw(0,0,FBuffer)
  else
  begin
    Init(FBuffer);
    BackgroundRefresh(FBuffer);//刷新背景
    FOffset:=0;
    DrawTexts(FBuffer,FOffset);
    Canvas.Draw(0,0,FBuffer)
  end;
end;

procedure TQFScrollingText.MouseMove(Shift: TShiftState; X, Y: Integer);
var k:integer;
begin
  inherited MouseMove(Shift, X, Y);

  if Assigned(FHyperLink) then
  begin
    for k:=0 to high(FHyperLink) do
    begin
      if (y>abs(FOffset+FHyperLink[k].y1)) and (y<abs(FOffset+FHyperLink[k].y2)) and
         (x>FHyperLink[k].x1) and (x<FHyperLink[k].x2) then
      begin
        FActiveLine := FHyperLink[k].hs;
        break;
      end
      else
        FActiveLine:= -1;
    end;
  end;
  Cursor := crDefault;
  if (FActiveLine >= 0) and (FActiveLine < Lineno) and ActiveLineIsURL then
    Cursor := crHandPoint;
end;

procedure TQFScrollingText.DoTimer(Sender: TObject);
var k:integer;
begin
  if not Active then
    Exit;
  inc(FMV);
  Dec(FOffset, FStepSize);
  if Assigned(FHyperLink) then
  begin
    for k:=0 to high(FHyperLink) do
    begin
      FHyperLink[k].y1:=FHyperLink[k].y1-FStepSize;
      FHyperLink[k].y2:=FHyperLink[k].y2-FStepSize;
      //选中URL滚动范围超出URL行高时，取消选中的URL行号及恢复鼠标状态
      if FActiveLine<>-1 then
      begin
        if fmv> abs(FHyperLink[k].y1-FHyperLink[k].y2) then
        begin
          fmv:=0;
          Cursor := crDefault;
          FActiveLine:=-1;
        end;
      end;
    end;
  end;

  BackgroundRefresh(FBuffer);//刷新背景
  DrawTexts(FBuffer,0);
  if FOffset+FTextHeigth=0 then
    FOffset := FBuffer.Height;

  Invalidate;
end;

constructor TQFHorizontalScrollingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTextWidth:=0;
  FOffsetX:=0;
  OnPaint := @DrawScrollingText;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer:=@DoTimer;
  FTimer.Interval:=30;
end;

destructor TQFHorizontalScrollingText.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TQFHorizontalScrollingText.SetActive(const AValue: boolean);
begin
  FActive := AValue;
  if FActive then
  begin
    Init(FBuffer);
  end;
  FTimer.Enabled:=Active;
end;

procedure TQFHorizontalScrollingText.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    if FActive then
      FTimer.Enabled:=false;
    FLines.Assign(AValue);
    Init(FBuffer);
    if FActive then
      FTimer.Enabled:=FActive;
  end;
end;

procedure TQFHorizontalScrollingText.SetScrollingText(const AValue: string);
begin
  if (AValue <> FScrollingText) then
  begin
    FScrollingText:= AValue;
    Lines.Text:=AValue;
  end;
end;

procedure TQFHorizontalScrollingText.SetShowBackImage(AValue: Boolean);
begin
  if (AValue <> FShowBackImage) then
  begin
    FShowBackImage:=AValue;
    Init(FBuffer);
    FOffset:=0;
    HDrawTexts(FBuffer,FOffsetx,0);
    Canvas.Draw(0,0,FBuffer)
  end;
end;

procedure TQFHorizontalScrollingText.DrawScrollingText(Sender: TObject);
begin
  if Active then
    Canvas.Draw(0,0,FBuffer)
  else
  begin
    Init(FBuffer);
    BackgroundRefresh(FBuffer);//刷新背景
    FOffsetX:=0;
    HDrawTexts(FBuffer,FOffsetx,0);
    Canvas.Draw(0,0,FBuffer)
  end;
end;

procedure TQFHorizontalScrollingText.HDrawTexts(Buffer: TBitmap;x,y:integer);
var
  i,h: integer;
  str:string;
begin
  str:='';
  for i:=0 to Lineno-1 do
     str:=str+FLineList[i].str;

  Buffer.Canvas.Font.Size:=FLineList[0].FontSize;
  if FLineList[0].FontStyle=0 then
    Buffer.Canvas.Font.Style:=[];
  if FLineList[0].FontStyle=1 then
      Buffer.Canvas.Font.Style:=[fsBold];
  if FLineList[0].FontStyle=2 then
      Buffer.Canvas.Font.Style:=[fsStrikeOut];
  if FLineList[0].FontStyle=3 then
      Buffer.Canvas.Font.Style:=[fsItalic];
  if FLineList[0].FontStyle=4 then
      Buffer.Canvas.Font.Style:=[fsUnderline];
  Buffer.Canvas.Font.Color:=FLineList[0].FontColor;

  FTextWidth:=GetStringTextWidth(Buffer,str);
  h:=(Buffer.Canvas.GetTextHeight(str)+FGapY);
  h:=abs(h-Buffer.Height) div 2; //垂直居中
  DisplayChar(Buffer,x, y+h, str);
end;

procedure TQFHorizontalScrollingText.DoTimer(Sender: TObject);
var k:integer;
begin
  if not Active then
    Exit;

  Dec(FOffsetX, FStepSize);

  BackgroundRefresh(FBuffer);//刷新背景
  HDrawTexts(FBuffer,FOffsetX,0);
  if FOffsetX+FTextWidth=0 then
    FOffsetX := FBuffer.Width;

  Invalidate;
end;

constructor TQFRichView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnPaint := @DrawScrollingText;
  FStepSize := 10;
  FShowUrlBookMakeHint:=true;
end;

destructor TQFRichView.Destroy;
begin
  inherited Destroy;
end;

procedure TQFRichView.SetShowUrlBookMakeHint(Value : Boolean);
begin
  if FShowUrlBookMakeHint <> Value then
  begin
    FShowUrlBookMakeHint := Value;
  end;
end;

procedure TQFRichView.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    FLines.Assign(AValue);
    Init(FBuffer);
  end;
end;
procedure TQFRichView.WMMouseWheel(var Message: TLMMouseEvent);
var k:integer;
begin
  inherited WMMouseWheel(Message);

  self.ShowHint:=false;
  FActiveLine:= -1;
  FBMActiveStr:='';
  Cursor := crDefault;
  if Message.WheelDelta<0 then //up
  begin
    if abs(FOffset)<FTextHeigth-FBuffer.Height+FStepSize+35 then
    begin
      Dec(FOffset, FStepSize);
      if Assigned(FHyperLink) then
      begin
        for k:=0 to high(FHyperLink) do
        begin
          FHyperLink[k].y1:=FHyperLink[k].y1-FStepSize;
          FHyperLink[k].y2:=FHyperLink[k].y2-FStepSize;
        end;
      end;
      if Assigned(FBookMark1) then
      begin
        for k:=0 to high(FBookMark1) do
        begin
          FBookMark1[k].y1:=FBookMark1[k].y1-FStepSize;
          FBookMark1[k].y2:=FBookMark1[k].y2-FStepSize;
        end;
      end;
      if Assigned(FBookMark2) then
      begin
        for k:=0 to high(FBookMark2) do
        begin
          FBookMark2[k].y1:=FBookMark2[k].y1-FStepSize;
          FBookMark2[k].y2:=FBookMark2[k].y2-FStepSize;
        end;
      end;
    end;
  end
  else
  begin   //down
    if FOffset<0 then
    begin
      FOffset:=FOffset+FStepSize;
      if Assigned(FHyperLink) then
      begin
        for k:=0 to high(FHyperLink) do
        begin
          FHyperLink[k].y1:=FHyperLink[k].y1+FStepSize;
          FHyperLink[k].y2:=FHyperLink[k].y2+FStepSize;
        end;
      end;
      if Assigned(FBookMark1) then
      begin
        for k:=0 to high(FBookMark1) do
        begin
          FBookMark1[k].y1:=FBookMark1[k].y1+FStepSize;
          FBookMark1[k].y2:=FBookMark1[k].y2+FStepSize;
        end;
      end;
      if Assigned(FBookMark2) then
      begin
        for k:=0 to high(FBookMark2) do
        begin
          FBookMark2[k].y1:=FBookMark2[k].y1+FStepSize;
          FBookMark2[k].y2:=FBookMark2[k].y2+FStepSize;
        end;
      end;
    end;
  end;
  BackgroundRefresh(FBuffer);//刷新背景

  DrawTexts(FBuffer,0);
  Canvas.Draw(0,0,FBuffer);
end;

procedure TQFRichView.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  k,k1,oldy1,oldy2:integer;
  oldTextHeigth:integer;
begin
  if Button = mbLeft then
  begin
    // 处理左键按下
    FisLeftButtonDown := True;
    FinitialY := Y;
  end;

  if FBMActiveStr<>'' then //跳转到指定书签的位置
  begin
      if Assigned(FBookMark2) then
      begin
        oldy1:=0;
        oldy2:=0;
        for k:=0 to high(FBookMark2) do
        begin
          if FBookMark2[k].BookMark=FBMActiveStr then
          begin
            BackgroundRefresh(FBuffer);//刷新背景
            FOffset:=0;
            oldTextHeigth:=FTextHeigth;
            oldy1:=FBookMark2[k].y1;
            oldy2:=FBookMark2[k].y2;
            DrawTexts(FBuffer,-FBookMark2[k].y1);
            Canvas.Draw(0,0,FBuffer);
            FOffset:=-oldy1;
            FTextHeigth:=oldTextHeigth;
            for k1:=0 to high(FBookMark2) do
            begin
              FBookMark2[k1].y1:=-oldy1;
              FBookMark2[k1].y2:=-oldy2;
            end;
            break;
          end;
        end;
        if Assigned(FHyperLink) then
        begin
          for k:=0 to high(FHyperLink) do
          begin
            FHyperLink[k].y1:=FHyperLink[k].y1+oldy1;
            FHyperLink[k].y2:=FHyperLink[k].y2+oldy1;
          end;
        end;
      end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TQFRichView.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Button = mbLeft then
  begin
    // 处理左键释放
    FisLeftButtonDown := False;
  end;
end;

procedure TQFRichView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  movedY: Integer;//存储鼠标移动的距离
  k:integer;
begin
  inherited MouseMove(Shift, X, Y);

  self.ShowHint:=false;
  if Assigned(FHyperLink) then   //URL
  begin
    for k:=0 to high(FHyperLink) do
    begin
      if (y>abs(FOffset+FHyperLink[k].y1)) and (y<abs(FOffset+FHyperLink[k].y2)) and
         (x>FHyperLink[k].x1) and (x<FHyperLink[k].x2) then
      begin
        if FShowUrlBookMakeHint then
        begin
          self.Hint:='超链接地址:'+FHyperLink[k].URL;
          self.ShowHint:=true;
        end;
        FActiveLine := FHyperLink[k].hs;
        break;
      end
      else
        FActiveLine:= -1;
    end;
  end;
  if Assigned(FBookMark1) then  //书签
  begin
    for k:=0 to high(FBookMark1) do
    begin
      if (y>abs(FOffset+FBookMark1[k].y1)) and (y<abs(FOffset+FBookMark1[k].y2)) and
         (x>FBookMark1[k].x1) and (x<FBookMark1[k].x2) then
      begin
        FBMActiveLine := FBookMark1[k].hs;
        FBMActiveStr := FBookMark1[k].BookMark;
        if FShowUrlBookMakeHint then
        begin
          self.Hint:='书签名称:'+FBookMark1[k].BookMark;
          self.ShowHint:=true;
        end;
        break;
      end
      else
      begin
        FBMActiveStr:= '';
        FBMActiveLine:= -1;
      end;
    end;
  end;

  Cursor := crDefault;
  //URL
  if (FActiveLine >= 0) and (FActiveLine < Lineno) and ActiveLineIsURL then
    Cursor := crHandPoint;

  //书签
  if (FBMActiveLine >= 0) and (FBMActiveLine < Lineno) then
    Cursor := crHandPoint;

  if FisLeftButtonDown then
  begin
    movedY := Y - FinitialY; // 计算Y轴上的移动距离

    if movedY > 0 then
    begin
      // 鼠标向下移动
      if abs(FOffset)<(FTextHeigth-FBuffer.Height+35) then
      begin
        Dec(FOffset, abs(35));
        if Assigned(FHyperLink) then
        begin
          for k:=0 to high(FHyperLink) do
          begin
            FHyperLink[k].y1:=FHyperLink[k].y1-35;
            FHyperLink[k].y2:=FHyperLink[k].y2-35;
          end;
        end;
        if Assigned(FBookMark1) then
        begin
          for k:=0 to high(FBookMark1) do
          begin
            FBookMark1[k].y1:=FBookMark1[k].y1-35;
            FBookMark1[k].y2:=FBookMark1[k].y2-35;
          end;
        end;
        if Assigned(FBookMark2) then
        begin
          for k:=0 to high(FBookMark2) do
          begin
            FBookMark2[k].y1:=FBookMark2[k].y1-35;
            FBookMark2[k].y2:=FBookMark2[k].y2-35;
          end;
        end;
      end;
    end
    else
    if movedY < 0 then
    begin
      // 鼠标向上移动
      if FOffset<0 then
      begin
        inc(FOffset, abs(35));
        if Assigned(FHyperLink) then
        begin
          for k:=0 to high(FHyperLink) do
          begin
            FHyperLink[k].y1:=FHyperLink[k].y1+35;
            FHyperLink[k].y2:=FHyperLink[k].y2+35;
          end;
        end;
        if Assigned(FBookMark1) then
        begin
          for k:=0 to high(FBookMark1) do
          begin
            FBookMark1[k].y1:=FBookMark1[k].y1+35;
            FBookMark1[k].y2:=FBookMark1[k].y2+35;
          end;
        end;
        if Assigned(FBookMark2) then
        begin
          for k:=0 to high(FBookMark2) do
          begin
            FBookMark2[k].y1:=FBookMark2[k].y1+35;
            FBookMark2[k].y2:=FBookMark2[k].y2+35;
          end;
        end;
      end;
    end;

    BackgroundRefresh(FBuffer);//刷新背景
    DrawTexts(FBuffer,0);
    Canvas.Draw(0,0,FBuffer);
  end;
end;

procedure TQFRichView.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
end;

procedure TQFRichView.DrawScrollingText(Sender: TObject);
begin
  Init(FBuffer);
  FOffset:=0;
  DrawTexts(FBuffer,FOffset);
  Canvas.Draw(0,0,FBuffer)
end;

procedure TQFRichView.SavePicture(Files:string);
var
  im:TImage;
  SaveBuffer: TBitmap;
  oldFOffset:integer;
begin
  SaveBuffer:=TBitmap.Create;
  SaveBuffer.Height:=FTextHeigth;
  SaveBuffer.Width:=FBuffer.Width;

  Init(FBuffer);
  FRect.Width:=Width;
  FRect.Height:=FTextHeigth;
  FRect.Left:=0;
  FRect.Top:=0;
  if trim(FBackImageFile)<>'' then
  begin
    if FBackgroundImage<>nil then
      SaveBuffer.Canvas.StretchDraw(FRect,FBackgroundImage.Picture.Bitmap)
    else
    begin
      with SaveBuffer.Canvas do
      begin
        Brush.Color := FColor;
        Brush.Style := bsSolid;
        FillRect(FRect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
                          //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
      end;
    end;
  end
  else
  begin
    with SaveBuffer.Canvas do
    begin
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      FillRect(FRect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
                        //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
    end;
  end;
  oldFOffset:=FOffset;
  FOffset:=0;
  DrawTexts(SaveBuffer,0);

  im:=TImage.Create(nil);
  im.Picture.Jpeg.Assign(SaveBuffer);
  im.Picture.SaveToFile(Files);
  im.Free;

  SaveBuffer.Free;
  FOffset:=oldFOffset;
end;

procedure TQFRichView.Print;
var
  PrintBuffer: TBitmap;
  oldFOffset:integer;
  MyPrinter: TPrinter;
begin
  Init(FBuffer);

  MyPrinter := Printer; // 获取打印机对象

  PrintBuffer:=TBitmap.Create;
  PrintBuffer.Height:=MyPrinter.PaperSize.PaperRect.WorkRect.Height;//FTextHeigth;
  PrintBuffer.Width:=MyPrinter.PaperSize.PaperRect.WorkRect.Width;//FBuffer.Width;


  FRect.Width:=MyPrinter.PaperSize.PaperRect.WorkRect.Width;//Width;
  FRect.Height:=MyPrinter.PaperSize.PaperRect.WorkRect.Height;//FTextHeigth;
  FRect.Left:=0;
  FRect.Top:=0;
  if trim(FBackImageFile)<>'' then
  begin
    if FBackgroundImage<>nil then
      PrintBuffer.Canvas.StretchDraw(FRect,FBackgroundImage.Picture.Bitmap)
    else
    begin
      with PrintBuffer.Canvas do
      begin
        Brush.Color := FColor;
        Brush.Style := bsSolid;
        FillRect(FRect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
                          //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
      end;
    end;
  end
  else
  begin
    with PrintBuffer.Canvas do
    begin
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      FillRect(FRect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
                        //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
    end;
  end;
  oldFOffset:=FOffset;
  FOffset:=0;
  DrawTexts(PrintBuffer,0);

  MyPrinter.BeginDoc;
  try
    MyPrinter.Canvas.CopyRect( // 将位图内容复制到打印机画布上
      Classes.Rect(
      0,
      0,
      MyPrinter.PaperSize.PaperRect.WorkRect.Width, //页可打印宽度
      MyPrinter.PaperSize.PaperRect.WorkRect.Height),//页可打印高度
      //Classes.Rect(0, 0, MyPrinter.PaperSize.Width, MyPrinter.PaperSize.Height),
      PrintBuffer.Canvas, Classes.Rect(0, 0, PrintBuffer.Width, PrintBuffer.Height)
    );
  finally
    MyPrinter.EndDoc; // 结束文档
  end;

  PrintBuffer.Free;
  FOffset:=oldFOffset;
end;

procedure TQFRichView.PageHeader;
begin
  Init(FBuffer);
  FOffset:=0;
  DrawTexts(FBuffer,FOffset);
  Canvas.Draw(0,0,FBuffer)
end;

procedure TQFRichView.PageFooter;
begin
  Init(FBuffer);
  FOffset:=-(FTextHeigth-FBuffer.Height+70);
  DrawTexts(FBuffer,0);
  Canvas.Draw(0,0,FBuffer);
end;

constructor TQFGridPanelComponent.Create(AOwner: TComponent);
var m:TMenuItem;
begin
  inherited Create(AOwner);

  //if (csDesigning in ComponentState) or Assigned(LazarusIDE) then
  //  FPOpupMenu:=TPopupMenu.Create(AOwner)
  //else
    FPopupMenu:=TPopupMenu.Create(self);
  InitPopupMenu;
  FEditFocusColor:=clWhite;
  FEditFontFocusColor:=clBlack;
  FTableWidth:=0;
  FTableHeight:=0;
  FColCount:= 5;
  FRowCount:= 5;
  FColSizing:= true;
  FRowSizing:=true;
  FCellLineColor := clSilver;
  FCellLineStyle := psSolid;
  FOldR.Left:=0;
  FOldR.Top:=0;
  FOldR.Width:=0;
  FOldR.Height:=0;
  FOldSelectRow:=0;
  FOldSelectCol:=0;
  FRun:=0;
  FGap:=5;
  FBorder:=true;
  FRowHeight:=0;
  FSelectCol:=-1;
  FSelectRow:=-1;
  //Lines.Add('||||||');
  //Lines.Add('|:-:|:-:|:-:|:-:|:-:|');
  //Lines.Add('||||||');
  //Lines.Add('||||||');
  //Lines.Add('||||||');
  //Lines.Add('||||||');
  //Lines.Add('||||||');
  OnPaint := @DisplayTable;
  FOldFontName:=FBuffer.Canvas.Font.Name;
  FConfigFileName:='QFGridPanelConfig.cfg';
  FIsShowBackImage:=false;
end;

destructor TQFGridPanelComponent.Destroy;
begin
  inherited Destroy;
  if FControlsList<>nil then
    FControlsList.Free;
  //FPopupMenu.Free;
end;

//复制单元格
procedure TQFGridPanelComponent.CopyCells;
begin
  FCopyTable.Gap:= FTable[FSelectRow,FSelectCol].Gap;
  FCopyTable.Align:= FTable[FSelectRow,FSelectCol].Align;
  FCopyTable.Color:= FTable[FSelectRow,FSelectCol].Color;
  FCopyTable.ColMerge:= FTable[FSelectRow,FSelectCol].ColMerge;
  FCopyTable.RowMerge:= FTable[FSelectRow,FSelectCol].RowMerge;
  FCopyTable.ComponentDataFieldName:= FTable[FSelectRow,FSelectCol].ComponentDataFieldName;
  FCopyTable.ComponentDataSource:= FTable[FSelectRow,FSelectCol].ComponentDataSource;
  FCopyTable.ComponentName:= FTable[FSelectRow,FSelectCol].ComponentName;
  FCopyTable.ComponentName:= FTable[FSelectRow,FSelectCol].ComponentType;
  FCopyTable.DispType:= FTable[FSelectRow,FSelectCol].DispType;
  FCopyTable.DrawBottom:= FTable[FSelectRow,FSelectCol].DrawBottom;
  FCopyTable.DrawLeft:= FTable[FSelectRow,FSelectCol].DrawLeft;
  FCopyTable.DrawRight:= FTable[FSelectRow,FSelectCol].DrawRight;
  FCopyTable.DrawTop:= FTable[FSelectRow,FSelectCol].DrawTop;
  FCopyTable.FontColor:= FTable[FSelectRow,FSelectCol].FontColor;
  FCopyTable.FontName:= FTable[FSelectRow,FSelectCol].FontName;
  FCopyTable.FontSize:= FTable[FSelectRow,FSelectCol].FontSize;
  FCopyTable.FontStyle:= FTable[FSelectRow,FSelectCol].FontStyle;

  FCopyTable.LeftLineStyle:= FTable[FSelectRow,FSelectCol].LeftLineStyle;
  FCopyTable.LineStyle:= FTable[FSelectRow,FSelectCol].LineStyle;
  FCopyTable.RightLineStyle:= FTable[FSelectRow,FSelectCol].RightLineStyle;
  FCopyTable.TopLineStyle:= FTable[FSelectRow,FSelectCol].TopLineStyle;
  FCopyTable.BottomLineStyle:= FTable[FSelectRow,FSelectCol].BottomLineStyle;
  FCopyTable.Visible:= FTable[FSelectRow,FSelectCol].Visible;
  FCopyTable.Width:= FTable[FSelectRow,FSelectCol].Width;
  FCopyTable.str:= FTable[FSelectRow,FSelectCol].str;
  FPOpupMenu.Items[1].Enabled:=true;  //粘贴菜单项设置true
end;

//粘贴单元格
procedure TQFGridPanelComponent.PasteCells;
begin
  FTable[FSelectRow,FSelectCol].Gap:=FCopyTable.Gap;
  FTable[FSelectRow,FSelectCol].Align:=FCopyTable.Align;
  FTable[FSelectRow,FSelectCol].Color:=FCopyTable.Color;
  FTable[FSelectRow,FSelectCol].ColMerge:=FCopyTable.ColMerge;
  FTable[FSelectRow,FSelectCol].RowMerge:= FCopyTable.RowMerge;
  FTable[FSelectRow,FSelectCol].ComponentDataFieldName:=FCopyTable.ComponentDataFieldName;
  FTable[FSelectRow,FSelectCol].ComponentDataSource:=FCopyTable.ComponentDataSource;
  FTable[FSelectRow,FSelectCol].ComponentName:=FCopyTable.ComponentName;
  FTable[FSelectRow,FSelectCol].ComponentType:=FCopyTable.ComponentName;
  FTable[FSelectRow,FSelectCol].DispType:=FCopyTable.DispType;
  FTable[FSelectRow,FSelectCol].DrawBottom:=FCopyTable.DrawBottom;
  FTable[FSelectRow,FSelectCol].DrawLeft:=FCopyTable.DrawLeft;
  FTable[FSelectRow,FSelectCol].DrawRight:=FCopyTable.DrawRight;
  FTable[FSelectRow,FSelectCol].DrawTop:=FCopyTable.DrawTop;
  FTable[FSelectRow,FSelectCol].FontColor:=FCopyTable.FontColor;
  FTable[FSelectRow,FSelectCol].FontName:=FCopyTable.FontName;
  FTable[FSelectRow,FSelectCol].FontSize:=FCopyTable.FontSize;
  FTable[FSelectRow,FSelectCol].FontStyle:=FCopyTable.FontStyle;

  FTable[FSelectRow,FSelectCol].LeftLineStyle:=FCopyTable.LeftLineStyle;
  FTable[FSelectRow,FSelectCol].LineStyle:=FCopyTable.LineStyle;
  FTable[FSelectRow,FSelectCol].RightLineStyle:=FCopyTable.RightLineStyle;
  FCopyTable.TopLineStyle:= FTable[FSelectRow,FSelectCol].TopLineStyle;
  FTable[FSelectRow,FSelectCol].BottomLineStyle:=FCopyTable.BottomLineStyle;
  FTable[FSelectRow,FSelectCol].Visible:=FCopyTable.Visible;
  FTable[FSelectRow,FSelectCol].Width:=FCopyTable.Width;
  FTable[FSelectRow,FSelectCol].str:=FCopyTable.str;
  DrawTable;
end;

//清除单元格
procedure TQFGridPanelComponent.ClearCells;
begin
  FTable[FSelectRow,FSelectCol].Gap:=0;
  FTable[FSelectRow,FSelectCol].Align:=calNone;
  FTable[FSelectRow,FSelectCol].Color:=clWhite;
  //FTable[FSelectRow,FSelectCol].ColMerge:=0;
  //FTable[FSelectRow,FSelectCol].RowMerge:=0;
  FTable[FSelectRow,FSelectCol].ComponentDataFieldName:=nil;
  FTable[FSelectRow,FSelectCol].ComponentDataSource:=nil;
  FTable[FSelectRow,FSelectCol].ComponentName:='';
  FTable[FSelectRow,FSelectCol].ComponentType:='';
  FTable[FSelectRow,FSelectCol].DispType:=dtText;
  FTable[FSelectRow,FSelectCol].DrawBottom:=true;
  FTable[FSelectRow,FSelectCol].DrawLeft:=true;
  FTable[FSelectRow,FSelectCol].DrawRight:=true;
  FTable[FSelectRow,FSelectCol].DrawTop:=true;
  FTable[FSelectRow,FSelectCol].FontColor:=clBlack;
  FTable[FSelectRow,FSelectCol].FontName:='';
  FTable[FSelectRow,FSelectCol].FontSize:=0;
  FTable[FSelectRow,FSelectCol].FontStyle:=cfsNone;

  FTable[FSelectRow,FSelectCol].LeftLineStyle:=psSolid;
  FTable[FSelectRow,FSelectCol].LineStyle:=psSolid;
  FTable[FSelectRow,FSelectCol].RightLineStyle:=psSolid;
  FTable[FSelectRow,FSelectCol].TopLineStyle:=psSolid;
  FTable[FSelectRow,FSelectCol].BottomLineStyle:=psSolid;
  FTable[FSelectRow,FSelectCol].Visible:=true;
  FTable[FSelectRow,FSelectCol].str:='';
  TableMerge;
  DrawTable;
end;

//设置单元格
procedure TQFGridPanelComponent.SetCellProper;
var
  i,j:integer;
  Index,tmp,err: Integer;
  CellProper :TQFCellProper;
  oldRowCount,oldColCount:integer;
begin
  GetControlsList;
  CellProper := TQFCellProper.Create(Self);

  setlength(CellProper.GTable,FRowCount+1,FColCount+2);

  oldRowCount:= FRowCount;
  oldColCount:= FColCount;
  //根据当前单元格信息设置
  CellProper.ColEdit.Text:=FColCount.ToString;
  CellProper.RowEdit.Text:=FRowCount.ToString;
  CellProper.LineColor.Color:=FCellLineColor;
  CellProper.CbxLineStyle.ItemIndex:=ord(FCellLineStyle);
  CellProper.ComboBox1.Items.Clear;
  CellProper.ComboBox1.text:='';
  CellProper.ComboBox1.Items.Assign(FControlsList);
  CellProper.ComboBox1.ItemIndex:=
     CellProper.ComboBox1.Items.IndexOf(FTable[FSelectRow,FSelectCol].ComponentName);
  CellProper.StringGrid1.Row:=FSelectRow;
  CellProper.StringGrid1.Col:=FSelectCol-1;
  CellProper.CellTextEdit.Text:=FTable[FSelectRow,FSelectCol].str;
  CellProper.StatusBar1.Panels[0].Text:='行:'+(FSelectRow+1).ToString+'  列:'+(FSelectCol).ToString;
  CellProper.StatusBar1.Panels[1].Text:=FTable[FSelectRow,FSelectCol].str;//StringGrid1.Cells[col,row];

  CellProper.oldColMerge:=FTable[FSelectRow,FSelectCol].ColMerge;
  CellProper.oldRowMerge:=FTable[FSelectRow,FSelectCol].RowMerge;

  CellProper.CellGapEdit.Text:=FTable[FSelectRow,FSelectCol].Gap.ToString;
  if FTable[FSelectRow,FSelectCol].Align=calNone then FTable[FSelectRow,FSelectCol].Align:=calClient;
  if FTable[FSelectRow,FSelectCol].Align>calNone then
    CellProper.CbxHAlign.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].Align)-1;

  if FTable[FSelectRow,FSelectCol].DispType=dtText then
    CellProper.CbxCellType.ItemIndex:=0;//文字
  if FTable[FSelectRow,FSelectCol].DispType=dtPict then
    CellProper.CbxCellType.ItemIndex:=1;//图像
  if FTable[FSelectRow,FSelectCol].DispType=dtComponent then
    CellProper.CbxCellType.ItemIndex:=2;//控件
  CellProper.RowMerge.text:=FTable[FSelectRow,FSelectCol].RowMerge.ToString;
  CellProper.ColMerge.text:=FTable[FSelectRow,FSelectCol].ColMerge.ToString;
  CellProper.ChkColSizing.Checked:=self.FColSizing;
  CellProper.ChkRowSizing.Checked:=self.FRowSizing;
  CellProper.ColWidthEdit.Text:=FColWidth.ToString;
  CellProper.RowHeightEdit.Text:=FRowHeight.ToString;
  CellProper.EditFontSize.Text:=FTable[FSelectRow,FSelectCol].FontSize.ToString;
  CellProper.LbxFontName.ItemIndex:=CellProper.LbxFontName.Items.IndexOf(FTable[FSelectRow,FSelectCol].FontName);
  CellProper.PanelFontPreview1.Font.Name:=FTable[FSelectRow,FSelectCol].FontName;
  CellProper.LbxFontSize.ItemIndex:=CellProper.LbxFontSize.Items.IndexOf(FTable[FSelectRow,FSelectCol].FontSize.ToString);
  CellProper.PanelFontColor.Color:=FTable[FSelectRow,FSelectCol].FontColor;
  CellProper.PanelFontPreview1.Font.Color:=FTable[FSelectRow,FSelectCol].FontColor;

  CellProper.GapEdit.Text:=FGap.ToString;
  CellProper.DrawBottomLine.Checked:=FTable[FSelectRow,FSelectCol].DrawBottom;
  CellProper.DrawLeftLine.Checked:=FTable[FSelectRow,FSelectCol].DrawLeft;
  CellProper.DrawRightLine.Checked:=FTable[FSelectRow,FSelectCol].DrawRight;
  CellProper.DrawTopLine.Checked:=FTable[FSelectRow,FSelectCol].DrawTop;
  CellProper.LeftLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].LeftLineStyle);
  CellProper.RightLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].RightLineStyle);
  CellProper.BottomLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].BottomLineStyle);
  CellProper.TopLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].TopLineStyle);
  //根据控件的FRowCount和FColCount设置StringGrid的行和列
  CellProper.StringGrid1.RowCount:=FRowCount;
  CellProper.StringGrid1.ColCount:=FColCount;

  CellProper.EditFontFocusColor.Color:=FEditFontFocusColor;
  CellProper.EditFocusColor.Color:=FEditFocusColor;
  CellProper.BackImageFile.Text:=FBackImageFile;
  CellProper.ShowBackImage.Checked:=FShowBackImage;
  CellProper.TableBorder.Checked:=FBorder;

  for i:=0 to FRowCount-1 do
  begin
    //CellProper.StringGrid1.RowHeights[i]:=FRowHeight;
    for j:=0 to FColCount do
    begin
      CellProper.GTable[i,j].Gap:=FTable[i,j].Gap;
      CellProper.GTable[i,j].Align:=FTable[i,j].Align;
      CellProper.GTable[i,j].BottomLineStyle:=FTable[i,j].BottomLineStyle;
      CellProper.GTable[i,j].Color:=FTable[i,j].Color;
      CellProper.GTable[i,j].ColMerge:=FTable[i,j].ColMerge;
      CellProper.GTable[i,j].RowMerge:=FTable[i,j].RowMerge;
      CellProper.GTable[i,j].ComponentDataFieldName:=FTable[i,j].ComponentDataFieldName;
      CellProper.GTable[i,j].ComponentName:=FTable[i,j].ComponentName;
      CellProper.GTable[i,j].ComponentDataSource:=FTable[i,j].ComponentDataSource;
      CellProper.GTable[i,j].ComponentType:=FTable[i,j].ComponentType;
      CellProper.GTable[i,j].DispType:=FTable[i,j].DispType;
      CellProper.GTable[i,j].DrawBottom:=FTable[i,j].DrawBottom;
      CellProper.GTable[i,j].DrawLeft:=FTable[i,j].DrawLeft;
      CellProper.GTable[i,j].DrawRight:=FTable[i,j].DrawRight;
      CellProper.GTable[i,j].DrawTop:=FTable[i,j].DrawTop;
      CellProper.GTable[i,j].FontColor:=FTable[i,j].FontColor;
      CellProper.GTable[i,j].FontName:=FTable[i,j].FontName;
      CellProper.GTable[i,j].FontSize:=FTable[i,j].FontSize;
      CellProper.GTable[i,j].FontStyle:=FTable[i,j].FontStyle;
      CellProper.GTable[i,j].Height:=FTable[i,j].Height;
      CellProper.GTable[i,j].LeftLineStyle:=FTable[i,j].LeftLineStyle;
      CellProper.GTable[i,j].LineStyle:=FTable[i,j].LineStyle;
      CellProper.GTable[i,j].RightLineStyle:=FTable[i,j].RightLineStyle;
      CellProper.GTable[i,j].str:=FTable[i,j].str;
      CellProper.GTable[i,j].TopLineStyle:=FTable[i,j].TopLineStyle;
      CellProper.GTable[i,j].Visible:=FTable[i,j].Visible;
      CellProper.GTable[i,j].Width:=FTable[i,j].Width;
      CellProper.GTable[i,j].x:=FTable[i,j].x;
      CellProper.GTable[i,j].y:=FTable[i,j].y;
      if j>0 then
      CellProper.StringGrid1.cells[j-1,i]:=FTable[i,j].str;
      //CellProper.StringGrid1.ColWidths[j]:=FColWidth;

    end;
  end;
  ////////////////////////////////
  //单元格设置后
  if CellProper.ShowModal=mrOk then
  begin
    FEditFontFocusColor:=CellProper.EditFontFocusColor.Color;
    FEditFocusColor:=CellProper.EditFocusColor.Color;
    FBackImageFile:=CellProper.BackImageFile.Text;
    FShowBackImage:=CellProper.ShowBackImage.Checked;
    FBorder:=CellProper.TableBorder.Checked;

    val(CellProper.GapEdit.Text,tmp,err);
    FGap:=tmp;

    val(CellProper.ColEdit.Text,tmp,err);
    if tmp<>FColCount then
      FColCount:=tmp;
    val(CellProper.RowEdit.Text,tmp,err);
    if tmp<>FRowCount then
      FRowCount:=tmp;

    if (FColCount<>oldColCount) or (FRowCount<>oldRowCount) then
    begin
      FTable:=nil;
      setlength(FTable,FRowcount+1,FColcount+1);
      if FRowcount<>oldRowcount then
      FRowHeight:=FBuffer.Height div FRowcount;
    end;
    if FCellLineColor<>CellProper.LineColor.Color then
    begin
      FCellLineColor:=CellProper.LineColor.Color;
      //DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
    end;
    if FCellLineStyle<>TFPPenStyle(CellProper.CbxLineStyle.ItemIndex) then
    begin
      FCellLineStyle:=TFPPenStyle(CellProper.CbxLineStyle.ItemIndex);
    end;

    FColSizing:=CellProper.ChkColSizing.Checked;
    FRowSizing:=CellProper.ChkRowSizing.Checked;

    for i:=0 to FRowCount-1 do
    begin
      for j:=0 to FColCount do
      begin
        FTable[i,j].Gap:=CellProper.GTable[i,j].Gap;
        FTable[i,j].Align:=CellProper.GTable[i,j].Align;
        FTable[i,j].BottomLineStyle:=CellProper.GTable[i,j].BottomLineStyle;
        FTable[i,j].Color:=CellProper.GTable[i,j].Color;
        FTable[i,j].ColMerge:=CellProper.GTable[i,j].ColMerge;
        FTable[i,j].ComponentDataFieldName:=CellProper.GTable[i,j].ComponentDataFieldName;
        FTable[i,j].ComponentName:=CellProper.GTable[i,j].ComponentName;
        FTable[i,j].ComponentDataSource:=CellProper.GTable[i,j].ComponentDataSource;
        FTable[i,j].ComponentType:=CellProper.GTable[i,j].ComponentType;
        FTable[i,j].DispType:=CellProper.GTable[i,j].DispType;
        FTable[i,j].DrawBottom:=CellProper.GTable[i,j].DrawBottom;
        FTable[i,j].DrawLeft:=CellProper.GTable[i,j].DrawLeft;
        FTable[i,j].DrawRight:=CellProper.GTable[i,j].DrawRight;
        FTable[i,j].DrawTop:=CellProper.GTable[i,j].DrawTop;
        FTable[i,j].FontColor:=CellProper.GTable[i,j].FontColor;
        FTable[i,j].FontName:=CellProper.GTable[i,j].FontName;
        FTable[i,j].FontSize:=CellProper.GTable[i,j].FontSize;
        FTable[i,j].FontStyle:=CellProper.GTable[i,j].FontStyle;
        FTable[i,j].Height:=CellProper.GTable[i,j].Height;
        FTable[i,j].LeftLineStyle:=CellProper.GTable[i,j].LeftLineStyle;
        FTable[i,j].LineStyle:=CellProper.GTable[i,j].LineStyle;
        FTable[i,j].RightLineStyle:=CellProper.GTable[i,j].RightLineStyle;
        FTable[i,j].RowMerge:=CellProper.GTable[i,j].RowMerge;
        FTable[i,j].TopLineStyle:=CellProper.GTable[i,j].TopLineStyle;
        FTable[i,j].Visible:=true;//CellProper.GTable[i,j].Visible;
        FTable[i,j].Width:=CellProper.GTable[i,j].Width;
        FTable[i,j].x:=CellProper.GTable[i,j].x;
        FTable[i,j].y:=CellProper.GTable[i,j].y;
        FTable[i,j].str:=CellProper.GTable[i,j].str
     end;
    end;
    FOldR.Left:=0;
    FOldR.Top:=0;
    FOldR.Width:=0;
    FOldR.Height:=0;
    FOldSelectRow:=0;
    FOldSelectCol:=0;

    TableMerge;
    SaveQFConfig;
    DrawTable;
    //Invalidate;

  end;
  CellProper.Free;
end;

procedure TQFGridPanelComponent.MenuItemClick(Sender: TObject);
begin
  case TStMenuItemTag(TMenuItem(Sender).Tag) of
    mtCopy :
      CopyCells;
    mtPaste :
      PasteCells;
    mtClearCells :
      ClearCells;
    mtSetCellProp :
      SetCellProper;
  end;
end;

// 初始化弹出式菜单
procedure TQFGridPanelComponent.InitPopupMenu;
var
  AMenuItem: TMenuItem;
begin
    AMenuItem := TMenuItem.Create(FPOpupMenu);
    AMenuItem.Caption := '复制(&C)';
    AMenuItem.Tag := Ord(mtCopy);
    AMenuItem.OnClick := @MenuItemClick;
    FPOpupMenu.Items.Add(AMenuItem);

    AMenuItem := TMenuItem.Create(FPOpupMenu);
    AMenuItem.Caption := '粘贴(&P)';
    AMenuItem.Tag := Ord(mtPaste);
    AMenuItem.OnClick := @MenuItemClick;
    AMenuItem.Enabled:=false;
    FPOpupMenu.Items.Add(AMenuItem);

    AMenuItem := TMenuItem.Create(FPOpupMenu);
    AMenuItem.Caption := '-';
    FPOpupMenu.Items.Add(AMenuItem);

    AMenuItem := TMenuItem.Create(FPOpupMenu);
    AMenuItem.Caption := '清除单元格内容';
    AMenuItem.Tag := Ord(mtClearCells);
    AMenuItem.OnClick := @MenuItemClick;
    FPOpupMenu.Items.Add(AMenuItem);

    AMenuItem := TMenuItem.Create(FPOpupMenu);
    AMenuItem.Caption := '-';
    FPOpupMenu.Items.Add(AMenuItem);

    AMenuItem := TMenuItem.Create(FPOpupMenu);
    AMenuItem.Caption := '设置单元格';
    AMenuItem.Tag := Ord(mtSetCellProp);
    AMenuItem.OnClick := @MenuItemClick;
    FPOpupMenu.Items.Add(AMenuItem);
end;

procedure TQFGridPanelComponent.EditEnter(Sender: TObject);
begin
  //isCell((Sender as TControl).Left,(Sender as TControl).Top,FCurrentR);
  //isCell((Sender as TControl).Left,(Sender as TControl).Top,FoldR);
  //DrawRect(FCurrentR,clred,1,FSelectRow,FSelectCol);
  if sender is TDBEdit then
  begin
    (sender as TDBEdit).Color:=FEditFocusColor;
    (sender as TDBEdit).Font.Color:=FEditFontFocusColor;
  end;
  if sender is TEdit then
  begin
    (sender as TEdit).Color:=FEditFocusColor;
    (sender as TEdit).Font.Color:=FEditFontFocusColor;
  end;
end;

procedure TQFGridPanelComponent.EditExit(Sender: TObject);
begin
  //isCell((Sender as TControl).Left,(Sender as TControl).Top,FoldR);
  //DrawRect(FoldR,FCellLineColor,1,FSelectRow,FSelectCol,true);
  if sender is TDBEdit then
  begin
    (sender as TDBEdit).Color:=FOldEditFocusColor;
    (sender as TDBEdit).Font.Color:=FOldEditFontFocusColor;
  end;
  if sender is TEdit then
  begin
    (sender as TEdit).Color:=FOldEditFocusColor;
    (sender as TEdit).Font.Color:=FOldEditFontFocusColor;
  end;
end;

procedure TQFGridPanelComponent.DoOnChangeBounds;
var
  WidthFactor,HeightFactor:double;
  NewColWidth,NewRowHeight,row,col:integer;
begin
  inherited DoOnChangeBounds;

  FOldSelectRow:=0;
  FOldSelectCol:=0;
  FOldR.Left:=0;
  FOldR.Top:=0;
  FOldR.Width:=0;
  FOldR.Height:=0;
  NewColWidth:=0;
  NewRowHeight:=0;
  if FRun=1 then
  begin
    FRun:=0;
    if FColCount<>0 then
      NewColWidth:= FBuffer.Width div FColCount;
    if FRowCount<>0 then
      NewRowHeight:= FBuffer.Height div FRowCount;
    if (NewColWidth<> FColWidth) or  (NewRowHeight<>FRowHeight) then
    begin
      WidthFactor:=1;
      HeightFactor:=1;
      if FColWidth<>0 then
        WidthFactor:= NewColWidth / FColWidth;
      if FRowHeight<>0 then
        HeightFactor:= NewRowHeight / FRowHeight;
      for row:=0 to FRowCount-1 do
      begin
        for col:=0 to FColCount do
        begin
           FTable[row,col].x:=round(FTable[row,col].x * WidthFactor);
           FTable[row,col].Width:=round(FTable[row,col].Width * WidthFactor);
           FTable[row,col].y:=round(FTable[row,col].y * HeightFactor);
           FTable[row,col].height:=round(FTable[row,col].height * HeightFactor);
        end;
      end;
      FColWidth:= NewColWidth;
      FRowHeight:=NewRowHeight;
      if (FRowHeight<>0) and (NewColWidth<>0) then
        DrawTable;
    end;
  end;
end;

procedure TQFGridPanelComponent.GetControlsList;
var
  i,j:integer;
begin
  FControlsList:=TStringlist.Create;
  for i:=0 to self.Parent.ControlCount-1 do
  begin
    if  UpperCase('QFGridPanelComponent')<>UpperCase(copy(self.Parent.Controls[i].Name,1,20)) then
    begin
      FControlsList.Add(self.Parent.Controls[i].Name);
    end;
    if  UpperCase('QFGridPanelComponent')=UpperCase(copy(self.Parent.Controls[i].Name,1,20)) then
    begin
      for j:=0 to self.ControlCount-1 do
        FControlsList.Add(self.Controls[j].Name);
    end;
  end;
end;

function TQFGridPanelComponent.LoadQFConfig(filename:string):boolean;
var
  jsonFile: TStringList;
begin
  Result:=false;
  try
    jsonFile:=TStringList.Create;
    if trim(filename)='' then
      filename:=FConfigFileName;

    if  FileExists(filename) then
    begin
      Result:=true;
      jsonFile.LoadFromFile(filename);
      loadjson(jsonFile.Text);
    end;
  finally
    jsonFile.Free;
  end;
end;

function TQFGridPanelComponent.LoadQFConfig:boolean;
var
  jsonFile: TStringList;
begin
  Result:=false;
  try
    jsonFile:=TStringList.Create;
    if  FileExists(FPathConfig+FConfigFileName) then
    begin
      Result:=true;
      jsonFile.LoadFromFile(FPathConfig+FConfigFileName);
      loadjson(jsonFile.Text);
    end;
  finally
    jsonFile.Free;
  end;
end;

procedure TQFGridPanelComponent.SaveQFConfig(filename:string);
begin
  if trim(filename)='' then
    filename:=FConfigFileName;
  savejson(filename);
end;

procedure TQFGridPanelComponent.SaveQFConfig;
begin
  savejson(FConfigFileName);
end;

procedure TQFGridPanelComponent.Refresh;
begin
  FRun:=0;
  if Init(FBuffer) then
  begin
    if FTablesl<>nil then
      GetTableInfo(0);
  end;
  if Tableiniti then
    DrawTable;
  //Canvas.Draw(0,0,FBuffer)
end;

procedure TQFGridPanelComponent.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    FRun:=0;
    FLines.Assign(AValue);
    if Init(FBuffer) then
    begin
      if FTablesl<>nil then
        GetTableInfo(0);
    end;
    if Tableiniti then
      DrawTable;
    //Canvas.Draw(0,0,FBuffer)
  end;
end;

procedure TQFGridPanelComponent.SetCellLineStyle(Value : TFPPenStyle);
begin
  if FCellLineStyle <> Value then
  begin
    FCellLineStyle := Value;
    Invalidate;
  end;
end;

procedure TQFGridPanelComponent.SetColCount(Value :integer);
begin
  if (FColCount<>Value) and (FColCount>-1)then
  begin
    FColCount := Value;
    if FColCount<>0 then
      FColWidth:= FBuffer.Width div FColCount;
    if FRowCount<>0 then
      FRowHeight:= FBuffer.Height div FRowCount;
    FTable:=nil;
    setlength(FTable,FRowcount+1,FColcount+1);
    TableMerge;
  end;
  DrawTable;
end;

procedure TQFGridPanelComponent.SetRowCount(Value :integer);
begin
  if (FRowCount <> Value) and (FRowCount>-1) then
  begin
    FRowCount := Value;
    if FColCount<>0 then
      FColWidth:= FBuffer.Width div FColCount;
    if FRowCount<>0 then
      FRowHeight:= FBuffer.Height div FRowCount;
    FTable:=nil;
    setlength(FTable,FRowcount+1,FColcount+1);
    TableMerge;
  end;
  //if FRowCount=-1 then Tableiniti;
  DrawTable;
end;

procedure TQFGridPanelComponent.SetCellLineColor(Value : TColor);
begin
  if FCellLineColor <> Value then
  begin
    FCellLineColor := Value;
    Invalidate;
  end;
end;

procedure TQFGridPanelComponent.SetEditFocusColor(Value : TColor);
begin
  if FEditFocusColor <> Value then
  begin
    FEditFocusColor := Value;
    Invalidate;
  end;
end;

procedure TQFGridPanelComponent.SetEditFontFocusColor(Value : TColor);
begin
  if FEditFontFocusColor <> Value then
  begin
    FEditFontFocusColor := Value;
    Invalidate;
  end;
end;

procedure TQFGridPanelComponent.DisplayTable(Sender: TObject);
begin
  if FRun=0 then
  begin
    if Init(FBuffer) then
    begin
      if FTablesl<>nil then
        GetTableInfo(0);
    end;
    Tableiniti;
  end;
  if FTable<>nil then
    DrawTable;
  //Canvas.Draw(0,0,FBuffer)
end;

procedure TQFGridPanelComponent.TableMerge;
var
  i,j:integer;
  row1,col1:integer;
  ss:real;
  oo,oo1:integer;
  NullTable:boolean;
  path:string;
begin
  if FColCount<>0 then
    FColWidth:= FBuffer.Width div FColCount;
  if FRowCount<>0 then
    FRowHeight:= FBuffer.Height div FRowCount;

  //计算合并后单元格的width和height
  for i:=0 to FRowCount do
  begin
    for j:=0 to FColCount do
    begin
      if FTable[i,j].Height=0 then
        FTable[i,j].Height:=FRowHeight;
      if FTable[i,j].Width=0 then
        FTable[i,j].Width:=FColWidth;
      FTable[i,j].DrawTop:=true;
      FTable[i,j].DrawLeft:=true;
      FTable[i,j].DrawBottom:=true;
      FTable[i,j].DrawRight:=true;
      FTable[i,j].LineStyle:=FCellLineStyle;
      if j=0 then
        FTable[i,j].Visible:=false ;

      //竖向(row)合并单元格
      if (FTable[i,j].RowMerge>0) and (FTable[i,j].ColMerge>0) then
      begin
        row1:=FTable[i,j].RowMerge+i-1;
        if row1>FRowCount then row1:=FRowCount;

        for oo:=i to row1 do
        begin
          col1:=FTable[i,j].ColMerge+j-1;
          if col1>FColCount then col1:=FColCount;
          FTable[i,j].Visible:=true;//将左上角单元格置为true
          ss:=1;
          if (FTable[i,j].Width<>FColWidth) and
             (FTable[i,j].Width<>FColWidth*FTable[i,j].ColMerge) then
            ss:=FTable[i,j].Width/(FColWidth*FTable[i,j].ColMerge);
          FTable[i,j].Width:=round(FColWidth*FTable[i,j].ColMerge*ss);
          FTable[i,j].Height:=FRowHeight*FTable[i,j].RowMerge;
          for oo1:=j to col1 do
          begin
            FTable[oo,oo1].Visible:=false;
          end;
        end;
      end
      else
      begin
        //竖向(row)合并单元格
        if FTable[i,j].RowMerge>0 then
        begin
          FTable[i,j].Height:=FRowHeight*FTable[i,j].RowMerge;
          row1:=FTable[i,j].RowMerge+i-1;
          if row1>FRowCount then row1:=FRowCount;
          for oo:=i+1 to row1 do
          begin
            FTable[oo,j].Visible:=false;
          end;
        end
        else
        //横向(col)合并单元格
        if FTable[i,j].ColMerge>0 then
        begin
          ss:=1;
          if (FTable[i,j].Width<>FColWidth) and
             (FTable[i,j].Width<>FColWidth*FTable[i,j].ColMerge) then
            ss:=FTable[i,j].Width/(FColWidth*FTable[i,j].ColMerge);
          FTable[i,j].Width:=round(FColWidth*FTable[i,j].ColMerge*ss);
          col1:=FTable[i,j].ColMerge+j-1;
          if col1>FColCount then col1:=FColCount;
          for oo:=j+1 to col1 do
          begin
            FTable[i,oo].Visible:=false;
          end;
        end;
      end;
    end;
  end;
end;

function TQFGridPanelComponent.Tableiniti:boolean;
var
  i,j,row,col:integer;
  texth:integer;//文字高度
  row1,col1:integer;
  oo,oo1:integer;
  NullTable:boolean;
  path:string;
  y,Index:integer;
begin
  if FColCount<>0 then
    FColWidth:= FBuffer.Width div FColCount;
  if FRowCount<>0 then
    FRowHeight:= FBuffer.Height div FRowCount;

  if  FileExists(FPathConfig+FConfigFileName) then//如果当前目录有FConfigFileName文件，则直接调用配置文件
    Result:= LoadQFConfig(FPathConfig+FConfigFileName)
  else
  begin
    Result:=false;
    index:=0;
    Result:=true;
    NullTable:=false;
    if FTablesl<>nil then
    begin
      //DeleteRecord(0);//删除第2行定义单元格的对齐格式
      row:=FTablesl[Index].row-1;
      col:=FTablesl[Index].col-1;
      FRowCount:=row;
      FColCount:=col;
    end
    else
    begin
      row:=FRowCount;
      col:=FColCount;
      if FTable=nil then
      begin
        NullTable:=true;
        setlength(FTable,FRowcount+1,FColcount+1);
      end;
    end;
    FBuffer.Canvas.Font.Style:=[fsBold];
    FBuffer.Canvas.Pen.Color:=FCellLineColor;//黑色画笔

    //重新计算合并后单元格的width和height
    for i:=0 to row-1 do
    begin
      for j:=0 to col do
      begin
        FTable[i,j].Height:=FRowHeight;
        FTable[i,j].Width:=FColWidth;
        FTable[i,j].DrawTop:=true;
        FTable[i,j].DrawLeft:=true;
        FTable[i,j].DrawBottom:=true;
        FTable[i,j].DrawRight:=true;
        FTable[i,j].LineStyle:=FCellLineStyle;
        if NullTable then
          FTable[i,j].Visible:=true;
        if j=0 then
          FTable[i,j].Visible:=false;

        //竖向(row)合并单元格
        if (FTable[i,j].RowMerge>0) and (FTable[i,j].ColMerge>0) then
        begin
          row1:=FTable[i,j].RowMerge+i-1;
          if row1>row then row1:=row;

          for oo:=i to row1 do
          begin
            col1:=FTable[i,j].ColMerge+j-1;
            if col1>col then col1:=col;
            for oo1:=j to col1 do
            begin
              FTable[oo,oo1].Visible:=false;
            end;
            FTable[i,j].Visible:=true;//将左上角单元格置为true
            FTable[i,j].Width:=FColWidth*FTable[i,j].ColMerge;
            FTable[i,j].Height:=FRowHeight*FTable[i,j].RowMerge;
          end;
        end
        else
        //竖向(row)合并单元格
        if FTable[i,j].RowMerge>0 then
        begin
          FTable[i,j].Height:=FRowHeight*FTable[i,j].RowMerge;
          row1:=FTable[i,j].RowMerge+i-1;
          if row1>row then row1:=row;
          for oo:=i+1 to row1 do
          begin
            FTable[oo,j].Visible:=false;
          end;
        end
        else
        //横向(col)合并单元格
        if FTable[i,j].ColMerge>0 then
        begin
          FTable[i,j].Width:=FColWidth*FTable[i,j].ColMerge;
          col1:=FTable[i,j].ColMerge+j-1;
          if col1>col then col1:=col;
          for oo:=j+1 to col1 do
          begin
            FTable[i,oo].Visible:=false;
          end;
        end;
      end;
    end;
  end;
end;

function TQFGridPanelComponent.FindChildControls(str:string):TControl;
var i,j:integer;
begin
  Result:=nil;

  for i:=0 to self.Parent.ControlCount-1 do
  begin
    if  UpperCase('QFGridPanelComponent')<>UpperCase(copy(self.Parent.Controls[i].Name,1,20)) then
    begin
      if UpperCase(self.Parent.Controls[i].Name) =str.ToUpper then
      begin
        Result:=self.Parent.Controls[i];
        break;
      end;
    end;
    if  UpperCase('QFGridPanelComponent')=UpperCase(copy(self.Parent.Controls[i].Name,1,20)) then
    begin
      for j:=0 to self.ControlCount-1 do
      begin
        if UpperCase(self.Controls[j].Name) =str.ToUpper then
        begin
          Result:=self.Controls[j];
          break;
        end;
      end;
    end;
  end;
end;

procedure TQFGridPanelComponent.DrawTable;
var
  i,j,w1,h1:integer;
  hg:integer;
  k:integer;
  x0,y0,x1,y1,y2:integer;
  Texth:integer;//文字高度
  TabStops:integer;
  Control: TControl;
  CellGap,Index,y:integer;
begin
  Index:=0;
  y:=0;
  BackgroundRefresh(FBuffer); //刷新背景
  FBuffer.Canvas.Font.Style:=[fsBold];

  Texth:= FBuffer.Canvas.TextHeight('国');

  FBuffer.Canvas.Pen.Color:=FCellLineColor;  //边框颜色
  FBuffer.Canvas.Pen.Style:=FCellLineStyle;  //边框线条样式

  //画单元格
  hg:=0;
  x0:=0;
  y0:=0;
  for i:=0 to FRowCount do
  begin
    x0:=0;
    for j:=1 to FColCount do
    begin
      if (j>1) and (FTable[i,j-1].Width=FColWidth) then
        x0:=x0+FTable[i,j-1].Width
      else x0:=x0+FTable[i,j-1].x;

      //if FTable[i,j-1].Height>FRowHeight then
      //  y0:=i*FRowHeight
      //else
      //  y0:=i*FTable[i,j-1].Height;

      if (j=1) and (i>0) then
        y0:=y0+FTable[i,j-1].Height;

      if FTable[i,j].Visible then
      begin

        FTable[i,j].x:=x0;
        FTable[i,j].y:=y0;

        x1:=x0+FTable[i,j].Width;
        y1:=y0+FTable[i,j].Height;

        if x0>=FBuffer.Width then
          x0:=FBuffer.Width-1;
        if x1>=FBuffer.Width then
          x1:=FBuffer.Width-1;
        if FTable[i,j].DrawTop then
        begin
          if FTable[i,j].TopLineStyle<>FCellLineStyle then
            FBuffer.Canvas.Pen.Style:=FTable[i,j].TopLineStyle
          else
            FBuffer.Canvas.Pen.Style:=FCellLineStyle;
          FBuffer.Canvas.Line(x0,y0,x1,y0);//顶横线
        end;
        if FTable[i,j].DrawRight then
        begin
          if FTable[i,j].RightLineStyle<>FCellLineStyle then
            FBuffer.Canvas.Pen.Style:=FTable[i,j].RightLineStyle
          else
            FBuffer.Canvas.Pen.Style:=FCellLineStyle;
          FBuffer.Canvas.Line(x1,y0,x1,y1);//右竖线
        end;
        if FTable[i,j].DrawBottom then
        begin
          if FTable[i,j].BottomLineStyle<>FCellLineStyle then
            FBuffer.Canvas.Pen.Style:=FTable[i,j].BottomLineStyle
          else
            FBuffer.Canvas.Pen.Style:=FCellLineStyle;
          FBuffer.Canvas.Line(x1,y1,x0,y1);//底横线
        end;
        if FTable[i,j].DrawLeft then
        begin
          if FTable[i,j].LeftLineStyle<>FCellLineStyle then
            FBuffer.Canvas.Pen.Style:=FTable[i,j].LeftLineStyle
          else
            FBuffer.Canvas.Pen.Style:=FCellLineStyle;
          FBuffer.Canvas.Line(x0,y1,x0,y0);//左竖线
        end;
      end;
    end;
  end;
  FTableWidth:=FColWidth*FColCount;//x1;
  FTableHeight:=y1;

  //开始画表格最外框边框
  FBuffer.Canvas.Pen.Color:=clBlack;//黑色画笔
  FBuffer.Canvas.Pen.Style:=psSolid;
  if FBorder then
  begin
    FBuffer.Canvas.Pen.Width:=2;
    FBuffer.Canvas.MoveTo(1,1);
    if FColWidth*FColCount>=FBuffer.Canvas.Width then
    begin
      FBuffer.Canvas.LineTo(FBuffer.Canvas.Width-1,1);  //顶
      FBuffer.Canvas.LineTo(FBuffer.Canvas.Width-1,FRowHeight*(FRowCount));//右;
    end
    else
    begin
      FBuffer.Canvas.LineTo(FColWidth*FColCount-1,1);
      FBuffer.Canvas.LineTo(FColWidth*FColCount-1,FRowHeight*(FRowCount));
    end;
    FBuffer.Canvas.LineTo(1,FRowHeight*(FRowCount)); //底
    FBuffer.Canvas.LineTo(1,1);//左
    FBuffer.Canvas.Pen.Width:=1;
  end;
  //结束画表格边框

  //绘表格内容（文字/图像/控件)
  FBuffer.Canvas.Brush.Style := bsClear;//透明文字
  TabStops:=0;

  x0:=0;
  y0:=0;
  for i:=0 to FRowCount do
  begin
    x0:=0;
    for j:=1 to FColCount do
    begin
      if (j=1) and (i>0) then
         y0:=y0 + FTable[i,j-1].Height;
      if FTable[i,j].Visible then
      begin
        if FTable[i,j].Gap=0 then
          CellGap:=FGap
        else
          CellGap:=FTable[i,j].Gap;
        if (FTable[i,j].DispType=dtText) or (FTable[i,j+1].DispType=dtBookmak1)
           or (FTable[i,j].DispType=dtBookmak2) or (FTable[i,j+1].DispType=dtLink) then  //显示文字
        begin
          //设置字体属性
          if FTable[i,j].FontName<>'' then
            FBuffer.Canvas.Font.Name:=FTable[i,j].FontName
          else
            FBuffer.Canvas.Font.Name:=FOldFontName;

          if FTable[i,j].FontStyle=cfsNone then FBuffer.Canvas.Font.Style:=[];
          if FTable[i,j].FontStyle=cfsBold then FBuffer.Canvas.Font.Style:=[fsBold];
          if FTable[i,j].FontStyle=cfsStrikeOut then FBuffer.Canvas.Font.Style:=[fsStrikeOut];
          if FTable[i,j].FontStyle=cfsItalic then FBuffer.Canvas.Font.Style:=[fsItalic];
          if FTable[i,j].FontStyle=cfsUnderline then FBuffer.Canvas.Font.Style:=[fsUnderline];
          if (FTable[i,j].DispType>dtPict) and (FTable[i,j].DispType<dtComponent) then
            FBuffer.Canvas.Font.Color:=clBlue //URL,bookmark
          else FBuffer.Canvas.Font.Color:=FTable[i,j].Color;
          FBuffer.Canvas.Font.Size:=FTable[i,j].FontSize;
          FBuffer.Canvas.Font.Color:=FTable[i,j].FontColor;
          Texth:= FBuffer.Canvas.TextHeight('国');
          //设置字体属性

          x0:=FTable[i,j].x;

          h1:=FRowHeight;
          w1:=FTable[i,j].Width;

          x1:=x0+CellGap; //居左
          if FTable[i,j].Align=calNone then
            FTable[i,j].Align:=calClient;//默认居中
          if FTable[i,j].Align=calLeft then
            x1:=x0+CellGap ;//居左
          if FTable[i,j].Align=calClient then
            x1:=x0+(FTable[i,j].Width-GetStringTextWidth(FBuffer,
                TruncationStr(FBuffer,FTable[i,j].str,FTable[i,j].Width))) div 2; //居中
          if FTable[i,j].Align=calRight then
             x1:=x0+(FTable[i,j].Width-GetStringTextWidth(FBuffer,
                 TruncationStr(FBuffer,FTable[i,j].str,FTable[i,j].Width)))-CellGap; //居右
          y2:=y0+ abs(FTable[i,j].Height- Texth) div 2;//垂直居中
          //在指定位置显示文字
          DisplayChar(FBuffer,x1+2, y2,TruncationStr(FBuffer,FTable[i,j].str,FTable[i,j].Width));
        end;
        if (FTable[i,j].DispType=dtComponent) and (FRun=0) then  //控件
        begin
          Control:=FindChildControls(FTable[i,j].ComponentName);//查找控件
          if Control<>nil then
          begin
            Control.BringToFront;//将控件置前
            Control.Parent:=self.Parent;
            Control.Width:=FTable[i,j].Width-CellGap*2;
            //if isComponent(Control) then
            if (Control is TMemo) or
               (Control is TButton) or
               (Control is TDBMemo) or
               (Control is TStringGrid) or
               (Control is TDBGrid) or
               (Control is TComboBox) or
               (Control is TDBListBox) or
               (Control is TListBox) or
               (Control is TDBComboBox) then
            begin
              Control.Height:= FTable[i,j].Height-CellGap*2;
            end;
            Control.Left:=FTable[i,j].x+CellGap+self.Left;
            y1:=(abs(Control.Height-FTable[i,j].Height) div 2)-CellGap;
            Control.Top:=FTable[i,j].y+CellGap+self.Top+y1;//垂直居中显示控件
            if isComponent(Control) then
            //if (Control is TEdit) or
            //   (Control is TDBEdit) or
            //   (Control is TMemo) or
            //   (Control is TDBMemo) or
            //   (Control is TComboBox) or
            //   (Control is TDBComboBox) or
            //   (Control is TDateEdit) or
            //   (Control is TDBDateEdit) or
            //   (Control is TButton)
            //   then
            begin
              FOldEditFocusColor:=TEdit(Control).Color;
              FOldEditFontFocusColor:=TEdit(Control).Font.Color;
              TEdit(Control).BorderStyle:=bsNone;
              TEdit(Control).TabOrder:= TabStops;
              TEdit(Control).OnEnter:=@EditEnter;
              TEdit(Control).OnExit:=@EditExit;
              inc(TabStops);
            end;
          end;
        end;
        if FTable[i,j].DispType=dtPict then  //显示图形
        begin
          x0:=FTable[i,j].x+1;
          y0:=FTable[i,j].y+1;

          if  FileExists(FPathConfig+FTable[i,j].str) then
          begin
            IMG.Picture.LoadFromFile(FPathConfig+FTable[i,j].str);
            //设置图像显示位置及尺寸（单元格大小）
            FRect.Top:=y0+CellGap;
            FRect.Left:=x0+CellGap;
            if FTable[i,j].Width<>0 then
              FRect.Width:=FTable[i,j].Width-CellGap*2
            else
              FRect.Width:=FColWidth-CellGap*2;
            if FTable[i,j].Height<>0 then
              FRect.Height:=FTable[i,j].Height-CellGap*2
            else
              FRect.Height:=FRowHeight-CellGap*2;
            FBuffer.Canvas.StretchDraw(FRect,img.Picture.Bitmap);
          end
          else
          begin
            //没找到图像文件
            DisplayChar(FBuffer,x0+2, y0,TruncationStr(FBuffer,'['+ExtractFileName(FTable[i,j].str+']'),FColWidth));
          end;
        end;
      end;
    end;
  end;
  FRun:=1;
  Canvas.Draw(0,0,FBuffer);
end;

function ControlToScreenX(AControl: TControl): Integer;
var
  ParentControl: TControl;
begin
  Result := AControl.Left;
  ParentControl := AControl.Parent;
  while (ParentControl <> nil) and not (ParentControl is TCustomForm) do
  begin
    Result := Result + ParentControl.Left;
    ParentControl := ParentControl.Parent;
  end;
  if ParentControl <> nil then
    Result := Result + TCustomForm(ParentControl).Left;
end;

function ControlToScreenY(AControl: TControl): Integer;
var
  ParentControl: TControl;
begin
  Result := AControl.Top;
  ParentControl := AControl.Parent;
  while (ParentControl <> nil) and not (ParentControl is TCustomForm) do
  begin
    Result := Result + ParentControl.Top;
    ParentControl := ParentControl.Parent;
  end;
  if ParentControl <> nil then
    Result := Result + TCustomForm(ParentControl).Top;
end;

procedure TQFGridPanelComponent.DrawRect(rect:TRect;colors:TColor;Linewidth,x,y:integer;RepaintRect:Boolean=false);
var
  CellControl:TControl;

  function isCellComponent(Rect:TRect;out Control: TControl):Boolean;
  var
    i,j,x0,x1,y0,y1:integer;
    parents:string;
  begin
    Result:=false;
    for i:=0 to FRowCount-1 do
    begin
      for j:=1 to FColCount do
      begin
        if (FTable[i,j].Visible)  then
        begin
          x0:=FTable[i,j].x;
          x1:=x0+FTable[i,j].Width;

          y0:=FTable[i,j].y;// i*FRowHeight;
          y1:=y0+FTable[i,j].Height;

          if (Rect.Left>=x0) and (Rect.Left+Rect.Width<=x1) and (Rect.top>=y0) and (Rect.top+Rect.Height<=y1) then
          begin
            if FTable[i,j].DispType=dtComponent then
            begin
              Control:= FindChildControls(FTable[i,j].ComponentName);
              Result:=true;
            end;
            Break;
          end;
        end;
      end;
    end;
  end;
begin
  Canvas.Pen.Width:=Linewidth;
  if isCellComponent(rect,CellControl) then //判断画框是不否有控件
  begin
    FOldBrushColor:=self.Color;
    if colors=FCellLineColor then
      Canvas.Brush.Color:=FOldBrushColor
    else
      Canvas.Brush.Color:=FEditFocusColor;
    if not FIsShowBackImage then //没用背景图片时，填充
      Canvas.FillRect(rect);
    if CellControl<>nil then
    begin
      if isComponent(CellControl) then
      begin
        FOldEditFocusColor:=TEdit(CellControl).Color;
        FOldEditFontFocusColor:=TEdit(CellControl).Font.Color;
        TEdit(CellControl).Color:=Canvas.Brush.Color;
      end;
    end;
    //Canvas.Brush.Color:=FoldEditFocusColor;
  end;
  Canvas.Pen.Color:=colors;
  Canvas.MoveTo(rect.Left,rect.Top);
  if rect.Left+rect.Width>=FTableWidth-1 then
  rect.Width:=rect.Width-1;

  //顶
  if (rect.Top=0) and (FBorder) then
  begin
    Canvas.Pen.Style:=FTable[x,y].TopLineStyle;
    //Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=3;
    Canvas.LineTo(rect.Left+rect.Width,rect.Top);  //顶
  end
  else
  begin
    Canvas.Pen.Width:=1;
    Canvas.Pen.Style:=FTable[x,y].TopLineStyle;
    if RepaintRect then//重绘单元格
    begin
      Canvas.Pen.Color:=FCellLineColor;
      if FTable[x,y].DrawTop then
        Canvas.LineTo(rect.Left+rect.Width,rect.Top);  //顶
    end
    else
    begin
      //Canvas.Pen.Style:=FTable[x,y].TopLineStyle;
      //Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
      Canvas.LineTo(rect.Left+rect.Width,rect.Top);  //顶
    end;
  end;

  //右;
  if (rect.Left+rect.Width>=FTableWidth-1)  and (FBorder) then
  begin
    Canvas.Pen.Style:=FTable[x,y].RightLineStyle;
    //Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=2;
    Canvas.LineTo(rect.Left+rect.Width,rect.Top+rect.Height);//右;
  end
  else
  begin
    Canvas.Pen.Width:=1;
    Canvas.Pen.Style:=FTable[x,y].RightLineStyle;
    if RepaintRect then
    begin
      Canvas.Pen.Color:=FCellLineColor;
      if FTable[x,y].DrawRight then
        Canvas.LineTo(rect.Left+rect.Width,rect.Top+rect.Height);//右;
    end
    else
    begin
      //Canvas.Pen.Style:=FTable[x,y].RightLineStyle;
      //Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
      Canvas.LineTo(rect.Left+rect.Width,rect.Top+rect.Height);//右;
    end;
  end;

  //底
  if (rect.Top+rect.Height=FTableHeight) and (FBorder)  then
  begin
    Canvas.Pen.Style:=FTable[x,y].BottomLineStyle;
    //Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=2;
    Canvas.LineTo(rect.Left,rect.Top+rect.Height); //底
  end
  else
  begin
    Canvas.Pen.Width:=1;
    Canvas.Pen.Style:=FTable[x,y].BottomLineStyle;
    if RepaintRect then
    begin
      Canvas.Pen.Color:=FCellLineColor;
      if FTable[x,y].DrawBottom then
        Canvas.LineTo(rect.Left,rect.Top+rect.Height); //底
    end
    else
    begin
      //Canvas.Pen.Style:=FTable[x,y].BottomLineStyle;
      //Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
      Canvas.LineTo(rect.Left,rect.Top+rect.Height); //底
    end;
  end;

  //左
  if (rect.Left=0) and (FBorder)  then
  begin
    Canvas.Pen.Style:=FTable[x,y].LeftLineStyle;
    //Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=2;
    Canvas.LineTo(rect.Left,rect.Top);//左
  end
  else
  begin
    Canvas.Pen.Width:=1;
    Canvas.Pen.Style:=FTable[x,y].LeftLineStyle;
    if RepaintRect then
    begin
      Canvas.Pen.Color:=FCellLineColor;
      if FTable[x,y].DrawLeft then
        Canvas.LineTo(rect.Left,rect.Top);//左
    end
    else
    begin
      //Canvas.Pen.Style:=FTable[x,y].LeftLineStyle;
      //Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
      Canvas.LineTo(rect.Left,rect.Top);//左
    end;
  end;
  //Canvas.Draw(0,0,FBuffer);
end;

function TQFGridPanelComponent.isComponent(Control:TControl):Boolean;
begin
  if (Control is TEdit) or
     (Control is TDBEdit) or
     (Control is TMemo) or
     (Control is TDBMemo) or
     (Control is TComboBox) or
     (Control is TDBComboBox) or
     (Control is TDateEdit) or
     (Control is TDBDateEdit) or
     (Control is TButton) then
     Result:=true
     else
     Result:=false;
end;

function TQFGridPanelComponent.isCell(x,y:integer;out Rect:TRect):Boolean;
var
  i,j,x0,x1,y0,y1:integer;
begin
  Result:=false;
  Rect.Left:=0;
  Rect.Top:=0;
  Rect.Width:=0;
  Rect.Height:=0;
  for i:=0 to FRowCount-1 do
  begin
    for j:=1 to FColCount do
    begin
      if (FTable[i,j].Visible)  then
      begin
        x0:=FTable[i,j].x;
        x1:=x0+FTable[i,j].Width;

        y0:=FTable[i,j].y;// i*FRowHeight;
        y1:=y0+FTable[i,j].Height;

        if (x>=x0) and (x<=x1) and (y>=y0) and (y<=y1) then
        begin
          Rect.Left:=x0;
          Rect.Top:=y0;
          Rect.Width:=FTable[i,j].Width;
          Rect.Height:=FTable[i,j].Height;
          FSelectCol:=j;
          FSelectRow:=i;
          Result:=true;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TQFGridPanelComponent.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    // 处理左键按下
    if (FResultCursor=crHSplit)or (FResultCursor=crVSplit)then //当调整光标时记录x,y坐标
    begin
      FisLeftButtonDown := True;
      FMouseDownXY.X := X;
      FMouseDownXY.Y := Y;
      if (FSelectRow>=0) and (FSelectCol>=0) then
      begin
        if FTable[FSelectRow,FSelectCol].DispType=dtComponent then
          DrawRect(FCurrentR,clred,1,FSelectRow,FSelectCol)
        else
          DrawRect(FCurrentR,clBlue,1,FSelectRow,FSelectCol);
      end;
    end;
  end;
end;

procedure TQFGridPanelComponent.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var movedX:integer;
  movedY:integer;
  i,j:integer;
  X1,w1,w2:integer;
begin
  if FisLeftButtonDown then  //按下鼠标左键
  begin
    //=================调整单元格高度======================
    if FResultCursor=crVSplit then
    begin
      FOldSelectRow:=0;
      FOldSelectCol:=0;
      FOldR.Left:=0;
      FOldR.Top:=0;
      FOldR.Width:=0;
      FOldR.Height:=0;
      movedY := Y - FMouseDownXY.Y; // 计算X轴上下移动距离
      //FRowHeight:=FRowHeight+movedY;

      FRun:=0;
      //DrawTable;
      //Canvas.Draw(0,0,FBuffer)
    end;
    //=================调整单元格高度======================


    //=================调整单元格宽度======================
    if FResultCursor=crHSplit then
    begin
      FOldSelectRow:=0;
      FOldSelectCol:=0;
      FOldR.Left:=0;
      FOldR.Top:=0;
      FOldR.Width:=0;
      FOldR.Height:=0;
      movedX := X - FMouseDownXY.X; // 计算Y轴水平移动距离
      if ssShift in Shift then//整列调整宽度
      begin
        if FSelectCol=1 then
        begin
          x1:=FTable[FSelectRow,FSelectCol+1].x;
          w1:=FTable[FSelectRow,FSelectCol].Width;
          w2:=FTable[FSelectRow,FSelectCol+1].Width;
          j:=FSelectCol+1;
        end
        else
        begin
          x1:=FTable[FSelectRow,FSelectCol].x;
          w1:=FTable[FSelectRow,FSelectCol].Width;
          w2:=FTable[FSelectRow,FSelectCol-1].Width;
          j:=FSelectCol;
        end;
        for i:=0 to FRowCount-1 do
        begin
          if ((FMouseDownXY.x>=FTable[i,j].x-2) and (FMouseDownXY.x<=FTable[i,j].x+2)) or
             ((FMouseDownXY.x>=FTable[i,j].x-2+FTable[i,j].Width) and (FMouseDownXY.x<=FTable[i,j].x+2++FTable[i,j].Width))
             and (FTable[i,j].Visible) then
          begin
            if movedX>0 then
            begin
              if FTable[i,FSelectCol-1].x+FTable[i,FSelectCol-1].Width+movedX<FTableWidth then
              begin
                if FTable[i,FSelectCol-1].Width<>FTableWidth then
                  FTable[i,FSelectCol-1].Width:=FTable[i,FSelectCol-1].Width+movedX;
              end;

              if FTable[i,FSelectCol].x+FTable[i,FSelectCol].Width-movedX=FTableWidth then
              begin
                if FTable[i,FSelectCol].Width<>FTableWidth then
                  FTable[i,FSelectCol].Width:=FTable[i,FSelectCol].Width+movedX
              end
              else
              if FTable[i,FSelectCol].x+FTable[i,FSelectCol].Width-movedX<FTableWidth then
                FTable[i,FSelectCol].Width:=FTable[i,FSelectCol].Width-movedX;
              FTable[i,FSelectCol].x:=FTable[i,FSelectCol-1].x+movedX;
            end
            else
            begin
              //if FTable[i,FSelectCol].Width=w1 then
              begin
                if FTable[i,FSelectCol].x+ FTable[i,FSelectCol].Width=FTableWidth then
                begin
                  FTable[i,FSelectCol-1].Width:=FTable[i,FSelectCol-1].Width-abs(movedX);
                  FTable[i,FSelectCol].Width:=FTable[i,FSelectCol].Width+abs(movedX);
                  FTable[i,FSelectCol].x:=FTable[i,FSelectCol].x-abs(movedX);
                end
                else
                begin
                  if (FTable[i,FSelectCol].x+ FTable[i,FSelectCol].Width+abs(movedX)<FTableWidth) and
                     (FTable[i,FSelectCol].Width<> FTableWidth) then
                  begin
                    FTable[i,FSelectCol].Width:=FTable[i,FSelectCol].Width+movedX;
                    FTable[i,FSelectCol+1].Width:=FTable[i,FSelectCol+1].Width+abs(movedX);
                    FTable[i,FSelectCol].x:=FTable[i,FSelectCol].x-abs(movedX);
                  end;
                end;
              end;
            end;
          end;
        end;
      end
      else
      begin
        if movedX>0 then
        begin
          if FTable[FSelectRow,FSelectCol-1].x+FTable[FSelectRow,FSelectCol-1].Width+movedX<FTableWidth then
            FTable[FSelectRow,FSelectCol-1].Width:=FTable[FSelectRow,FSelectCol-1].Width+movedX;

          if FTable[FSelectRow,FSelectCol].x+FTable[FSelectRow,FSelectCol].Width-movedX=FTableWidth then
            FTable[FSelectRow,FSelectCol].Width:=FTable[FSelectRow,FSelectCol].Width+movedX
          else
          if FTable[FSelectRow,FSelectCol].x+FTable[FSelectRow,FSelectCol].Width-movedX<FTableWidth then
            FTable[FSelectRow,FSelectCol].Width:=FTable[FSelectRow,FSelectCol].Width-movedX;

          FTable[FSelectRow,FSelectCol].x:=FTable[FSelectRow,FSelectCol-1].x+movedX;
        end
        else
        begin
          if FTable[FSelectRow,FSelectCol].x+ FTable[FSelectRow,FSelectCol].Width=FTableWidth then
          begin
            FTable[FSelectRow,FSelectCol-1].Width:=FTable[FSelectRow,FSelectCol-1].Width-abs(movedX);
            FTable[FSelectRow,FSelectCol].Width:=FTable[FSelectRow,FSelectCol].Width+abs(movedX);
            FTable[FSelectRow,FSelectCol].x:=FTable[FSelectRow,FSelectCol].x-abs(movedX);
          end
          else
          begin
            if FTable[FSelectRow,FSelectCol].x+FTable[FSelectRow,FSelectCol].Width-abs(movedX)<FTableWidth then
              FTable[FSelectRow,FSelectCol].Width:=FTable[FSelectRow,FSelectCol].Width-abs(movedX);

            if FTable[FSelectRow,FSelectCol+1].x+ FTable[FSelectRow,FSelectCol+1].Width+abs(movedX)<FTableWidth then
              FTable[FSelectRow,FSelectCol+1].Width:=FTable[FSelectRow,FSelectCol+1].Width+abs(movedX);
            FTable[FSelectRow,FSelectCol+1].x:=FTable[FSelectRow,FSelectCol+1].x-abs(movedX);
          end;
        end;
      end;
      FRun:=0;
      DrawTable;
      Canvas.Draw(0,0,FBuffer)
    end;
    //=================调整单元格宽度======================
  end;
  if Button = mbLeft then
  begin
    // 处理左键释放
    FisLeftButtonDown := False;
    Cursor := crDefault;
    FResultCursor:=Cursor;
  end;
  if Button=mbRight then
  begin
    if (FSelectRow>-1) and (FSelectCol>-1) then
    begin
      if FTable[FSelectRow,FSelectCol].DispType=dtComponent then
        DrawRect(FCurrentR,clred,1,FSelectRow,FSelectCol)
      else
        DrawRect(FCurrentR,clBlue,1,FSelectRow,FSelectCol);
    end
    else
    begin
      FSelectRow:=0;
      FSelectCol:=0;
    end;
    FPopupMenu.PopUp(ControlToScreenX(self)+x,ControlToScreenY(self)+y);
  end;
  inherited MouseUP(Button,Shift, X, Y);
end;

procedure TQFGridPanelComponent.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i,j:integer;

  function isLine(x,y:integer;out i0,j0:integer):TCursor;
  var
    i,j,x0,x1,y0,y1:integer;
  begin
    Cursor:=crDefault;
    Result:=Cursor;
    for i:=0 to FRowCount-1 do
    begin
      for j:=1 to FColCount do
      begin
          if FTable[i,j].Visible then
          begin
            x0:=FTable[i,j].x;
            y0:=i*FRowHeight;
            x1:=x0+FTable[i,j].Width;
            y1:=y0+FTable[i,j].Height;
            if (x>=x0-2) and (x<=x0+2) and (y>y0) and (y<y1) then
            begin
              if FColSizing then
              begin
                Cursor :=crHSplit;
                Result:=Cursor;  //水平线
                i0:=i;
                j0:=j;
              end;
              Break;
            end;
            if (x>x0) and (x<x1) and (y>=y0-2) and (y<=y0+2) then
            begin
              if FRowSizing then
              begin
                Cursor := crVSplit; //垂线
                Result:=Cursor;
                i0:=i;
                j0:=j;
              end;
              Break;
            end
          end;
      end;
    end;
  end;
begin
  inherited MouseMove(Shift, X, Y);

  if FTable<>nil then
  begin
    if isCell(x,y,FCurrentR) then
    begin
      DrawRect(FoldR,FCellLineColor,1,FOldSelectRow,FOldSelectCol,true);
      FOldR:=FCurrentR;
      FOldSelectRow:=FSelectRow;
      FOldSelectCol:=FSelectCol;
      if FTable[FSelectRow,FSelectCol].DispType=dtComponent then
        DrawRect(FCurrentR,clred,1,FSelectRow,FSelectCol)
      else
        DrawRect(FCurrentR,clBlue,1,FSelectRow,FSelectCol);
    end;
    if FisLeftButtonDown then
    begin
      FCurrentR.Left:=0;
      FCurrentR.Top:=0;
      FCurrentR.Width:=0;
      FCurrentR.Width:=0;
      FResultCursor:=Cursor;
    end
    else
    begin
      FResultCursor:=isLine(x,y,FMoveRow,FMoveCol);
    end;
    if FResultCursor=crHSplit then
    begin
       //FMVLineX:=x;
       //Canvas.Pen.Color:=clNone;
       //Canvas.Line(x,0,x,y+Canvas.Height);
    end;
  end;
end;

procedure TQFGridPanelComponent.LoadJSON(jsonstr:string);
var
  jsonRoot: TJSONObject;
  jData : TJSONData;
  jItem,kItem,litem : TJSONData;
  i,j,k,no: Integer;
  str,tmpstr,Versions:string;
  V1,v2,v3,v4,err:integer;
  Versionnum:int64;
  //object_name: String;
  OldRowcount,OldColcount:integer;
begin
  jData := GetJSON(utf8toansi(jsonstr));
  jsonRoot:=TJSONObject(jData);

  OldRowcount:=FRowcount;
  OldColcount:=FColcount;
  if jsonRoot.get('ConfigName')='QFGridPanelComponentConfig' then
  begin
    Versions:= jsonRoot.get('Version');
    tmpstr:='';
    k:=0;
    for i:=1 to length(Versions) do
    begin
       if (Versions[i]='.') or (i=length(Versions)) then
       begin
         inc(k);
         if k=1 then
          val(tmpstr,v1,err);
         if k=2 then
          val(tmpstr,v2,err);
         if k=3 then
          val(tmpstr,v3,err);
         if k=4 then
         begin
           tmpstr:=tmpstr+Versions[i];
           val(tmpstr,v4,err);
         end;
         tmpstr:='';
       end
       else tmpstr:=tmpstr+Versions[i];
    end;
    VersionNum:=v1*1000000+v2*10000+v3*100+v4; //0.9.9.9==>90909
{
    for i := 0 to jData.Count - 1 do
    begin
      jItem := jData.Items[i];
}
    // find:
    //"QFGridPanelComponent1" : {
    jItem:= jsonRoot.find(self.Name);
    if jItem<>nil then  //找到QFGridPanelComponent1
    begin
      no:=0;
      for j := 0 to jItem.Count - 1 do  //row
      begin
        if j=0 then
        begin
          FRowcount:= TJSONObject(jItem).get('FRowcount');
          FColcount:=TJSONObject(jItem).get('FColcount');
          FCellLineColor:=TJSONObject(jItem).get('FCellLineColor');
          FColSizing:=TJSONObject(jItem).get('FColSizing');
          FRowSizing:=TJSONObject(jItem).get('FRowSizing');
          FCellLineStyle:=TJSONObject(jItem).get('FCellLineStyle');
          FColWidth:=TJSONObject(jItem).get('FColWidth');
          FRowHeight:=TJSONObject(jItem).get('FRowHeight');
          FTableWidth:=TJSONObject(jItem).get('FTableWidth');
          FTableHeight:=TJSONObject(jItem).get('FTableHeight');
          FGap:=TJSONObject(jItem).get('FGap');
          FBorder:=TJSONObject(jItem).get('FBorder');
          FEditFontFocusColor:=TJSONObject(jItem).get('FEditFontFocusColor');
          FEditFocusColor:=TJSONObject(jItem).get('FEditFocusColor');
          if VersionNum>00090909 then
          begin
            FBackImageFile:=TJSONObject(jItem).get('FBackImageFile');
            FShowBackImage:=TJSONObject(jItem).get('FShowBackImage');
          end;
          FTable:=nil;
          setlength(FTable,FRowcount+1,FColcount+1);
        end;
        str := TJSONObject(jItem).Names[j];
        if str.ToUpper='ROW0' then no:=j;

        kItem:=jItem.Items[j];
        for k := 0 to kItem.Count - 1 do //col
        begin
          lItem:=kItem.Items[k];

          FTable[j-no,k].x:=TJSONObject(lItem.Items[0]).Get('x');
          FTable[j-no,k].y:=TJSONObject(lItem.Items[0]).Get('y');
          if VersionNum>00090910 then
            FTable[j-no,k].gap:=TJSONObject(lItem.Items[0]).Get('Gap');
          FTable[j-no,k].Width:=TJSONObject(lItem.Items[0]).Get('Width');
          FTable[j-no,k].Height:=TJSONObject(lItem.Items[0]).Get('Height');
          FTable[j-no,k].ColMerge:=TJSONObject(lItem.Items[0]).Get('ColMerge');
          FTable[j-no,k].RowMerge:=TJSONObject(lItem.Items[0]).Get('RowMerge');
          FTable[j-no,k].DispType:=TJSONObject(lItem.Items[0]).Get('DispType');
          FTable[j-no,k].str:=TJSONObject(lItem.Items[0]).Get('str');
          FTable[j-no,k].Color:=TJSONObject(lItem.Items[0]).Get('Color');
          FTable[j-no,k].Align:=TJSONObject(lItem.Items[0]).Get('Align');
          FTable[j-no,k].FontName:=TJSONObject(lItem.Items[0]).Get('FontName');
          FTable[j-no,k].FontSize:=TJSONObject(lItem.Items[0]).Get('FontSize');
          FTable[j-no,k].FontStyle:=TJSONObject(lItem.Items[0]).Get('FontStyle');
          FTable[j-no,k].FontColor:=TJSONObject(lItem.Items[0]).Get('FontColor');
          FTable[j-no,k].ComponentType:=TJSONObject(lItem.Items[0]).Get('ComponentType');
          FTable[j-no,k].ComponentName:=TJSONObject(lItem.Items[0]).Get('ComponentName');
          FTable[j-no,k].Visible:=TJSONObject(lItem.Items[0]).Get('Visible');
          FTable[j-no,k].DrawTop:=TJSONObject(lItem.Items[0]).Get('DrawTop');
          FTable[j-no,k].DrawLeft:=TJSONObject(lItem.Items[0]).Get('DrawLeft');
          FTable[j-no,k].DrawBottom:=TJSONObject(lItem.Items[0]).Get('DrawBottom');
          FTable[j-no,k].DrawRight:=TJSONObject(lItem.Items[0]).Get('DrawRight');
          FTable[j-no,k].LineStyle:=TJSONObject(lItem.Items[0]).Get('LineStyle');
          FTable[j-no,k].TopLineStyle:=TJSONObject(lItem.Items[0]).Get('TopLineStyle');
          FTable[j-no,k].LeftLineStyle:=TJSONObject(lItem.Items[0]).Get('LeftLineStyle');
          FTable[j-no,k].BottomLineStyle:=TJSONObject(lItem.Items[0]).Get('BottomLineStyle');
          FTable[j-no,k].RightLineStyle:=TJSONObject(lItem.Items[0]).Get('RightLineStyle');
        end;
      end;
    end;
    Frun:=1;
    DoOnChangeBounds;//根据控件尺寸重新调整单元格大小
    Frun:=0;
  end;
  jData.Free;
  jsonRoot:=nil;
end;

procedure TQFGridPanelComponent.SaveJSON(files:string);
VAR
jsonRoot,jsoncomp, jsonGrid, jsonRow,  jsonParamObj: TJSONObject;
jsonCol: TJSONArray;
i,j: Integer;
savejsonfile: TStringList ;
str:string;
begin
  //创建一个新的JSON对象来写入数据
  jsonRoot := TJSONObject.Create;
  jsonRoot.Add('ConfigName', utf8toansi('QFGridPanelComponentConfig'));
  jsonRoot.Add('Version', Version);

  jsonGrid := TJSONObject.Create;
  jsonRoot.Add(self.Name, jsonGrid);//添加QFGridPanelComponent1

  jsonGrid.Add('ComponentName', self.Name);
  jsonGrid.Add('FRowcount', FRowcount);
  jsonGrid.Add('FColcount', FColcount);
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
      jsonParamObj.Add('x', FTable[i,j].x);
      jsonParamObj.Add('y', FTable[i,j].y);
      jsonParamObj.Add('Gap', FTable[i,j].Gap);
      jsonParamObj.Add('Width', FTable[i,j].Width);
      jsonParamObj.Add('Height', FTable[i,j].Height);
      jsonParamObj.Add('ColMerge', FTable[i,j].ColMerge);
      jsonParamObj.Add('RowMerge', FTable[i,j].RowMerge);
      jsonParamObj.Add('DispType', ord(FTable[i,j].DispType));
      jsonParamObj.Add('str', FTable[i,j].str);
      jsonParamObj.Add('Color', FTable[i,j].Color);
      jsonParamObj.Add('Align', ord(FTable[i,j].Align));
      jsonParamObj.Add('FontName', FTable[i,j].FontName);
      jsonParamObj.Add('FontSize', FTable[i,j].FontSize);
      jsonParamObj.Add('FontStyle', ord(FTable[i,j].FontStyle));
      jsonParamObj.Add('FontColor', FTable[i,j].FontColor);
      jsonParamObj.Add('ComponentType', FTable[i,j].ComponentType);
      jsonParamObj.Add('ComponentName', FTable[i,j].ComponentName);
      //jsonParamObj.Add('ComponentDataSource', TJSONObject.Create(FTable[i,j].ComponentDataSource));
      //jsonParamObj.Add('ComponentDataFieldName',TJSONObjectClass.Create(FTable[i,j].ComponentDataFieldName.ClassName));
      jsonParamObj.Add('Visible', FTable[i,j].Visible);
      jsonParamObj.Add('DrawTop', FTable[i,j].DrawTop);
      jsonParamObj.Add('DrawLeft', FTable[i,j].DrawLeft);
      jsonParamObj.Add('DrawBottom', FTable[i,j].DrawBottom);
      jsonParamObj.Add('DrawRight', FTable[i,j].DrawRight);
      jsonParamObj.Add('LineStyle', ord(FTable[i,j].LineStyle));
      jsonParamObj.Add('TopLineStyle', ord(FTable[i,j].TopLineStyle));
      jsonParamObj.Add('LeftLineStyle', ord(FTable[i,j].LeftLineStyle));
      jsonParamObj.Add('BottomLineStyle', ord(FTable[i,j].BottomLineStyle));
      jsonParamObj.Add('RightLineStyle', ord(FTable[i,j].RightLineStyle));

      jsonCol.Add(jsonParamObj);
      jsonParamObj:=nil;
      jsonCol:=nil;
    end;
  end;
  str:=jsonRoot.FormatJSON;
  savejsonfile:=TStringList.Create;
  savejsonfile.add(str);
  savejsonfile.SaveToFile(FPathConfig+files);
  savejsonfile.Free;

  jsonGrid:=nil;
  jsonRow:=nil;
  jsonRoot:=nil;
end;

//添加设计时鼠标右按弹出菜单
function EditQFGridPanel(AGrid: TQFGridPanelComponent): Boolean;
var
  i,j:integer;
  Index,tmp,err: Integer;
  CellProper :TQFCellProper;
  oldRowCount,oldColCount:integer;
begin
  AGrid.GetControlsList;
  CellProper := TQFCellProper.Create(Application);
  try
    setlength(CellProper.GTable,AGrid.FRowCount+1,AGrid.FColCount+2);
    AGrid.FSelectRow:=0;
    AGrid.FSelectCol:=1;
    oldRowCount:= AGrid.FRowCount;
    oldColCount:= AGrid.FColCount;
    //根据当前单元格信息设置
    CellProper.ColEdit.Text:=AGrid.FColCount.ToString;
    CellProper.RowEdit.Text:=AGrid.FRowCount.ToString;
    CellProper.LineColor.Color:=AGrid.FCellLineColor;
    CellProper.CbxLineStyle.ItemIndex:=ord(AGrid.FCellLineStyle);
    CellProper.ComboBox1.Items.Clear;
    CellProper.ComboBox1.text:='';
    CellProper.ComboBox1.Items.Assign(AGrid.FControlsList);
    CellProper.ComboBox1.ItemIndex:=
       CellProper.ComboBox1.Items.IndexOf(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].ComponentName);
    CellProper.StringGrid1.Row:=AGrid.FSelectRow;
    CellProper.StringGrid1.Col:=AGrid.FSelectCol-1;
    CellProper.CellTextEdit.Text:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].str;
    CellProper.StatusBar1.Panels[0].Text:='行:'+(AGrid.FSelectRow+1).ToString+'  列:'+(AGrid.FSelectCol).ToString;
    CellProper.StatusBar1.Panels[1].Text:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].str;//StringGrid1.Cells[col,row];

    CellProper.oldColMerge:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].ColMerge;
    CellProper.oldRowMerge:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].RowMerge;

    CellProper.CellGapEdit.Text:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].Gap.ToString;
    if AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].Align=calNone then AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].Align:=calClient;
    if AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].Align>calNone then
      CellProper.CbxHAlign.ItemIndex:=ord(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].Align)-1;

    if AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].DispType=dtText then
      CellProper.CbxCellType.ItemIndex:=0;//文字
    if AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].DispType=dtPict then
      CellProper.CbxCellType.ItemIndex:=1;//图像
    if AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].DispType=dtComponent then
      CellProper.CbxCellType.ItemIndex:=2;//控件
    CellProper.RowMerge.text:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].RowMerge.ToString;
    CellProper.ColMerge.text:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].ColMerge.ToString;

    CellProper.ChkColSizing.Checked:=AGrid.FColSizing;
    CellProper.ChkRowSizing.Checked:=AGrid.FRowSizing;
    CellProper.ColWidthEdit.Text:=AGrid.FColWidth.ToString;
    CellProper.RowHeightEdit.Text:=AGrid.FRowHeight.ToString;
    CellProper.EditFontSize.Text:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].FontSize.ToString;
    CellProper.LbxFontName.ItemIndex:=CellProper.LbxFontName.Items.IndexOf(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].FontName);
    CellProper.PanelFontPreview1.Font.Name:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].FontName;
    CellProper.LbxFontSize.ItemIndex:=CellProper.LbxFontSize.Items.IndexOf(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].FontSize.ToString);
    CellProper.PanelFontColor.Color:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].FontColor;
    CellProper.PanelFontPreview1.Font.Color:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].FontColor;

    CellProper.GapEdit.Text:=AGrid.FGap.ToString;
    CellProper.DrawBottomLine.Checked:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].DrawBottom;
    CellProper.DrawLeftLine.Checked:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].DrawLeft;
    CellProper.DrawRightLine.Checked:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].DrawRight;
    CellProper.DrawTopLine.Checked:=AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].DrawTop;
    CellProper.LeftLineStyle.ItemIndex:=ord(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].LeftLineStyle);
    CellProper.RightLineStyle.ItemIndex:=ord(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].RightLineStyle);
    CellProper.BottomLineStyle.ItemIndex:=ord(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].BottomLineStyle);
    CellProper.TopLineStyle.ItemIndex:=ord(AGrid.FTable[AGrid.FSelectRow,AGrid.FSelectCol].TopLineStyle);
    //根据控件的FRowCount和FColCount设置StringGrid的行和列
    CellProper.StringGrid1.RowCount:=AGrid.FRowCount;
    CellProper.StringGrid1.ColCount:=AGrid.FColCount;

    CellProper.EditFontFocusColor.Color:=AGrid.FEditFontFocusColor;
    CellProper.EditFocusColor.Color:=AGrid.FEditFocusColor;
    CellProper.BackImageFile.Text:=AGrid.FBackImageFile;
    CellProper.ShowBackImage.Checked:=AGrid.FShowBackImage;
    CellProper.TableBorder.Checked:=AGrid.FBorder;

    for i:=0 to AGrid.FRowCount-1 do
    begin
      //CellProper.StringGrid1.RowHeights[i]:=FRowHeight;
      for j:=0 to AGrid.FColCount do
      begin
        CellProper.GTable[i,j].Gap:=AGrid.FTable[i,j].Gap;
        CellProper.GTable[i,j].Align:=AGrid.FTable[i,j].Align;
        CellProper.GTable[i,j].BottomLineStyle:=AGrid.FTable[i,j].BottomLineStyle;
        CellProper.GTable[i,j].Color:=AGrid.FTable[i,j].Color;
        CellProper.GTable[i,j].ColMerge:=AGrid.FTable[i,j].ColMerge;
        CellProper.GTable[i,j].RowMerge:=AGrid.FTable[i,j].RowMerge;
        CellProper.GTable[i,j].ComponentDataFieldName:=AGrid.FTable[i,j].ComponentDataFieldName;
        CellProper.GTable[i,j].ComponentName:=AGrid.FTable[i,j].ComponentName;
        CellProper.GTable[i,j].ComponentDataSource:=AGrid.FTable[i,j].ComponentDataSource;
        CellProper.GTable[i,j].ComponentType:=AGrid.FTable[i,j].ComponentType;
        CellProper.GTable[i,j].DispType:=AGrid.FTable[i,j].DispType;
        CellProper.GTable[i,j].DrawBottom:=AGrid.FTable[i,j].DrawBottom;
        CellProper.GTable[i,j].DrawLeft:=AGrid.FTable[i,j].DrawLeft;
        CellProper.GTable[i,j].DrawRight:=AGrid.FTable[i,j].DrawRight;
        CellProper.GTable[i,j].DrawTop:=AGrid.FTable[i,j].DrawTop;
        CellProper.GTable[i,j].FontColor:=AGrid.FTable[i,j].FontColor;
        CellProper.GTable[i,j].FontName:=AGrid.FTable[i,j].FontName;
        CellProper.GTable[i,j].FontSize:=AGrid.FTable[i,j].FontSize;
        CellProper.GTable[i,j].FontStyle:=AGrid.FTable[i,j].FontStyle;
        CellProper.GTable[i,j].Height:=AGrid.FTable[i,j].Height;
        CellProper.GTable[i,j].LeftLineStyle:=AGrid.FTable[i,j].LeftLineStyle;
        CellProper.GTable[i,j].LineStyle:=AGrid.FTable[i,j].LineStyle;
        CellProper.GTable[i,j].RightLineStyle:=AGrid.FTable[i,j].RightLineStyle;
        CellProper.GTable[i,j].str:=AGrid.FTable[i,j].str;
        CellProper.GTable[i,j].TopLineStyle:=AGrid.FTable[i,j].TopLineStyle;
        CellProper.GTable[i,j].Visible:=AGrid.FTable[i,j].Visible;
        CellProper.GTable[i,j].Width:=AGrid.FTable[i,j].Width;
        CellProper.GTable[i,j].x:=AGrid.FTable[i,j].x;
        CellProper.GTable[i,j].y:=AGrid.FTable[i,j].y;
        if j>0 then
        CellProper.StringGrid1.cells[j-1,i]:=AGrid.FTable[i,j].str;
      end;
    end;

    ////////////////////////////////
    //单元格设置后
    ////////////////////////////////
    if CellProper.ShowModal = mrOk then
    begin
      AGrid.FEditFontFocusColor:=CellProper.EditFontFocusColor.Color;
      AGrid.FEditFocusColor:=CellProper.EditFocusColor.Color;
      AGrid.FBackImageFile:=CellProper.BackImageFile.Text;
      AGrid.FShowBackImage:=CellProper.ShowBackImage.Checked;
      AGrid.FBorder:=CellProper.TableBorder.Checked;

      val(CellProper.GapEdit.Text,tmp,err);
      AGrid.FGap:=tmp;

      val(CellProper.ColEdit.Text,tmp,err);
      if tmp<>AGrid.FColCount then
        AGrid.FColCount:=tmp;
      val(CellProper.RowEdit.Text,tmp,err);
      if tmp<>AGrid.FRowCount then
        AGrid.FRowCount:=tmp;

      if (AGrid.FColCount<>oldColCount) or (AGrid.FRowCount<>oldRowCount) then
      begin
        AGrid.FTable:=nil;
        setlength(AGrid.FTable,AGrid.FRowcount+1,AGrid.FColcount+1);
        if AGrid.FRowcount<>oldRowcount then
        AGrid.FRowHeight:=AGrid.FBuffer.Height div AGrid.FRowcount;
      end;
      if AGrid.FCellLineColor<>CellProper.LineColor.Color then
      begin
        AGrid.FCellLineColor:=CellProper.LineColor.Color;
        //DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
      end;
      if AGrid.FCellLineStyle<>TFPPenStyle(CellProper.CbxLineStyle.ItemIndex) then
      begin
        AGrid.FCellLineStyle:=TFPPenStyle(CellProper.CbxLineStyle.ItemIndex);
      end;

      AGrid.FColSizing:=CellProper.ChkColSizing.Checked;
      AGrid.FRowSizing:=CellProper.ChkRowSizing.Checked;

      for i:=0 to AGrid.FRowCount-1 do
      begin
        for j:=0 to AGrid.FColCount do
        begin
          AGrid.FTable[i,j].Gap:=CellProper.GTable[i,j].Gap;
          AGrid.FTable[i,j].Align:=CellProper.GTable[i,j].Align;
          AGrid.FTable[i,j].BottomLineStyle:=CellProper.GTable[i,j].BottomLineStyle;
          AGrid.FTable[i,j].Color:=CellProper.GTable[i,j].Color;
          AGrid.FTable[i,j].ColMerge:=CellProper.GTable[i,j].ColMerge;
          AGrid.FTable[i,j].ComponentDataFieldName:=CellProper.GTable[i,j].ComponentDataFieldName;
          AGrid.FTable[i,j].ComponentName:=CellProper.GTable[i,j].ComponentName;
          AGrid.FTable[i,j].ComponentDataSource:=CellProper.GTable[i,j].ComponentDataSource;
          AGrid.FTable[i,j].ComponentType:=CellProper.GTable[i,j].ComponentType;
          AGrid.FTable[i,j].DispType:=CellProper.GTable[i,j].DispType;
          AGrid.FTable[i,j].DrawBottom:=CellProper.GTable[i,j].DrawBottom;
          AGrid.FTable[i,j].DrawLeft:=CellProper.GTable[i,j].DrawLeft;
          AGrid.FTable[i,j].DrawRight:=CellProper.GTable[i,j].DrawRight;
          AGrid.FTable[i,j].DrawTop:=CellProper.GTable[i,j].DrawTop;
          AGrid.FTable[i,j].FontColor:=CellProper.GTable[i,j].FontColor;
          AGrid.FTable[i,j].FontName:=CellProper.GTable[i,j].FontName;
          AGrid.FTable[i,j].FontSize:=CellProper.GTable[i,j].FontSize;
          AGrid.FTable[i,j].FontStyle:=CellProper.GTable[i,j].FontStyle;
          AGrid.FTable[i,j].Height:=CellProper.GTable[i,j].Height;
          AGrid.FTable[i,j].LeftLineStyle:=CellProper.GTable[i,j].LeftLineStyle;
          AGrid.FTable[i,j].LineStyle:=CellProper.GTable[i,j].LineStyle;
          AGrid.FTable[i,j].RightLineStyle:=CellProper.GTable[i,j].RightLineStyle;
          AGrid.FTable[i,j].RowMerge:=CellProper.GTable[i,j].RowMerge;
          AGrid.FTable[i,j].TopLineStyle:=CellProper.GTable[i,j].TopLineStyle;
          AGrid.FTable[i,j].Visible:=true;
          AGrid.FTable[i,j].Width:=CellProper.GTable[i,j].Width;
          AGrid.FTable[i,j].x:=CellProper.GTable[i,j].x;
          AGrid.FTable[i,j].y:=CellProper.GTable[i,j].y;
          AGrid.FTable[i,j].str:=CellProper.GTable[i,j].str
       end;
      end;
      AGrid.FOldR.Left:=0;
      AGrid.FOldR.Top:=0;
      AGrid.FOldR.Width:=0;
      AGrid.FOldR.Height:=0;
      AGrid.FOldSelectRow:=0;
      AGrid.FOldSelectCol:=0;
      AGrid.TableMerge;
      AGrid.SaveQFConfig;
      AGrid.DrawTable;
      Result:=true;
    end
    else
      Result:=false;
  finally
    CellProper.Free;
  end;
end;

procedure TQFGridPanelComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
begin
  if Index = 0 then
  begin
    GetHook(Hook);
   if EditQFGridPanel(GetComponent as TQFGridPanelComponent) then
      if Assigned(Hook) then //控件内容有变化，则将IDE状态改为修改
        Hook.Modified(Self);
 end;
end;

function TQFGridPanelComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := 'QFGridPanelComponent Editor'
  else Result := '';
end;

function TQFGridPanelComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;
//添加设计时鼠标右按弹出菜单--end

initialization
RegisterComponentEditor(TQFGridPanelComponent, TQFGridPanelComponentEditor);

end.
