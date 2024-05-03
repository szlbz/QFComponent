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
控件包有2个控件：
QFComponent由秋风(QQ:315795176)开发的控件包,采用自定义的富文本格式，
集编辑、显示、导出和打印等功能。纯pascal代码，没使用额外的dll/so，
只需QFComponent.pas这个文件就可以实现文字渲染等功能，可跨平台使用。
1、TQFRichView：采用自定义的富文本格式，类RichView控件，支持超链接、
   书签跳转等丰富的功能，适合作为使用说明等用途；
2、TQFScrollingText：采用自定义的富文本格式，可实现图文的滚动；

说明：
1、控件的自定义富文件格式渲染等核心功能是秋风独立原创编写。
2、QFRichEditor参考了https://github.com/DreamVB编写的Markdown Editor界面及代码
3、滚动控件的滚动部分参考了lazarus about的思路。

使用以下特定符号定义文字（行）的属性：
[img]本地图像文件名称
[!]下划线
[@]删除线
[#]粗体
[$]斜体
[L]行居左
[C]行居中
[R]行居右
[C1]黑色
[C2]红色
[C3]黄色
[C4]绿色
[C5]蓝色
[S1]字体尺寸9
[S2]字体尺寸12
[S3]字体尺寸14
[S4]字体尺寸16
[S5]字体尺寸18
[LINE]分割线
[2LINE]双线条分割线
定义文字颜色、样式：
<sup>上标
<sub>下标
</sup>取消上标
</sub>取消下标
<C1>xxx</C>
<C1>黑色
<C2>红色
<C3>黄色
<C4>绿色
<C5>蓝色
</C>恢复原来的文字颜色
<!>下划线
<@>删除线
<#>加粗
</>恢复原来的文字样式
初步支持markdown格式的表格
图像格式支持：jpg,bmp,png等
可在windows和linux使用，已在龙芯电脑实测可用。
TQFRichView控件支持鼠标中键滚动及按鼠标左键然后上下移动鼠标实现快速滚动。
2024.03.09
*******************************************************************************}

unit QFComponent;

interface

uses
  Classes, SysUtils, Forms, Controls,  Graphics, ExtCtrls,Dialogs, Printers,
  OSPrinters,StdCtrls,DBCtrls, Menus,QFCellProper, DB,  FPCanvas,
  fpjson,lazutf8,PublicDefinition,
  lclintf, LazFileUtils,  LMessages,StrUtils,QFRichEdit;

const
  ReservedSpace = 1024;

  VerInfo = 'TQFGridPanelComponent' + #26;
  Version ='0.9.9.9';

type

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
     DispType:string[10];
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
  TStMenuItemTag = (mtCut, mtCopy, mtPaste,
                  mtInsertRow, mtInsertCol,
                  mtDeleteRow, mtDeleteCol,
                  mtClearCells, mtSetCellProp);

  TCustomText = class(TCustomControl)//TScrollingWinControl)//TCustomControl)
  private
    FRect:TRect;
    FMV:integer;
    FLineSpacing:integer;//行距
    FTextHeigth:integer;
    //FQFRE:TQFRichEditor;
    FisLeftButtonDown: Boolean; //鼠标左键按下标识
    TTHNO:integer;
    FTS:integer;//表格数量
    FBackgroundImage:TImage;
    FBackImageFile:string;
    FShowBackImage:boolean;
    FBookMark1:array of TQFBookMark;
    FBookMark2:array of TQFBookMark;
    FHyperLink:array of THyperLink;
    FTablesl:array of TTableSL;
    FTable:Array of Array of TCell;
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
    procedure Init(Buffer:TBitmap);
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
    function DrawTable(Buffer: TBitmap;Index, y:integer):integer;virtual;
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
    FinitialXY:Tpoint;//用于存储鼠标按下时的初始位置
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
    procedure Tableiniti(Buffer: TBitmap;colWidth,h,Index,y:integer);
    procedure DisplayTable(Sender: TObject);
    procedure SetCellLineColor(Value : TColor);
    procedure SetEditFocusColor(Value : TColor);
    procedure SetEditFontFocusColor(Value : TColor);
    procedure SetCellLineStyle(Value : TFPPenStyle);
    function FindChildControls(str:string):TControl;
    function DrawTable(Buffer: TBitmap;colWidth,h,Index, y:integer):integer;//override;
    procedure MenuItemClick(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure InitPopupMenu;
    procedure SetCellProp;
    procedure DrawRect(rect:TRect;colors:TColor;Linewidth,x,y:integer;RepaintRect:byte=0);
    procedure LoadJSON(jsonstr:string);
    procedure SaveJSON(files:string);
  protected
    procedure SetLines(const AValue: TStrings);override;
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure GetControlsList;
    procedure SaveFile(filename:string);
    procedure LoadFile(filename:string);
    procedure SaveFile;
    procedure LoadFile;
  published
    property RowHeight:integer read FRowHeight write FRowHeight;
    property Gap:integer read FGap write FGap;
    property Border:Boolean read FBorder write FBorder;
    property EditFontFocusColor:TColor read FEditFontFocusColor write SetEditFontFocusColor default clBlack;
    property EditFocusColor:TColor read FEditFocusColor write SetEditFocusColor default clWhite;
    property CellLineColor:TColor read FCellLineColor write SetCellLineColor default clSilver;
    property CellLineStyle:TFPPenStyle read FCellLineStyle write SetCellLineStyle default psSolid;
    property Font;
    property ColCount:integer read FColCount write FColCount default 5;
    property RowCount:integer read FRowCount write FRowCount default 5;
    property ColSizing:Boolean read FColSizing write FColSizing default true;
    property RowSizing:Boolean read FRowSizing write FRowSizing default true;
    property ConfigFileName:string read FConfigFileName write FConfigFileName;
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
  CellType.DispType:=0;
  CellType.FontStyle:=0;
  CellType.FontName:='';
  CellType.FontSize:=0;
  CellType.Color:=clBlack;
  CellType.ColSpan:=0;
  CellType.RowSpan:=0;
  CellType.str:=s;
  CellType.Visible:=true;
  CellType.URL:='';
  CellType.ComponentName:='';
  if pos('[L]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[L]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.Align :=1;
  end;
  if pos('[C]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[C]','',[rfReplaceAll,rfIgnoreCase]);
    CellType.str:=s;
    CellType.Align :=2;
  end;
  if pos('[R]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[R]','',[rfReplaceAll,rfIgnoreCase]);
    CellType.str:=s;
    CellType.Align :=3;
  end;
  if pos('[#]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[#]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=1;//[fsBold];
  end;
  if pos('[@]',s)>0 then
  begin
    s:=s.Replace('[@]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=2;//[fsStrikeOut];
  end;
  if pos('[$]',s)>0 then
  begin
    s:=s.Replace('[$]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=3;//[fsItalic];
  end;
  if pos('[!]',s)>0 then
  begin
    s:=s.Replace('[!]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.FontStyle :=4;//[fsUnderline];
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
    CellType.DispType:=0;
    CellType.Color := clBlue;
  end;
  if pos('[IMG]',s.ToUpper)>0 then
  begin
    s:=s.Replace('[IMG]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    CellType.str:=s;
    CellType.DispType:=1;
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
    CellType.DispType:=0;//控件类
  end;
  if (utf8Pos('</FONTSIZE>', s.ToUpper)>0) then
  begin
    s1:=s;
   // CellType.FontSize:=0;
    s1:=s1.Replace('</FONTSIZE>','',[rfReplaceAll, rfIgnoreCase]);
    CellType.str:=s1;
    CellType.DispType:=0;//控件类
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
    CellType.DispType:=5;//控件类
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
    CellType.DispType:=2;
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
    CellType.DispType:=3;
  end;
  if utf8pos('<COLSPAN=',s.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<COLSPAN=',s.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s) do
    begin
      if utf8copy(s,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s,FontPos1+9,FontPos2-FontPos1-9);
        val(tmp1,tmp,e);

        tmp1:=utf8copy(s,FontPos1,FontPos2-FontPos1+1);
        s:=s.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<COLSPAN=',s.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.ColSpan:=tmp;
    CellType.str:=s;
  end;
  if utf8pos('<ROWSPAN=',s.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<ROWSPAN=',s.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s) do
    begin
      if utf8copy(s,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(s,FontPos1+9,FontPos2-FontPos1-9);
        val(tmp1,tmp,e);

        tmp1:=utf8copy(s,FontPos1,FontPos2-FontPos1+1);
        s:=s.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<ROWSPAN=',s.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.RowSpan:=tmp;
    CellType.str:=s;
  end;
  if utf8pos('<FONT=',s.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<FONT=',s.ToUpper);
    i:=FontPos1;
    while i<= utf8length(s) do
    begin
      if utf8copy(s,i,1)='>' then
      begin
        FontPos2:=i;
        tmp2:=utf8copy(s,FontPos1+6,FontPos2-(FontPos1+6));
        tmp1:=utf8copy(s,FontPos1,FontPos2-FontPos1+1);
        s:=s.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<FONT=',s.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
    CellType.FontName:=tmp2;
    CellType.str:=s;
  end;
  if pos('</FONT>',s.ToUpper)>0 then
  begin
    s:=s.Replace('</FONT>','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
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
    CellType.DispType:=4;
  end
  else
  if (pos('HTTP://',s.ToUpper)>0) or (pos('HTTPS://',s.ToUpper)>0) then
  begin
    CellType.str:=s;
    CellType.DispType:=4;
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
  Result:=Result.Replace('</FONT>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</FONTSIZE>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<SUB>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUB>','',[rfReplaceAll, rfIgnoreCase]);
  if utf8pos('<FONT=',Result.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<FONT=',Result.ToUpper);
    i:=FontPos1;
    while i<= utf8length(Result) do
    begin
      if utf8copy(Result,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(Result,FontPos1,FontPos2-FontPos1+1);
        Result:=Result.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<FONT=',Result.ToUpper);
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
  if utf8pos('<COLSPAN=',Result.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<COLSPAN=',Result.ToUpper);
    i:=FontPos1;
    while i<= utf8length(Result) do
    begin
      if utf8copy(Result,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(Result,FontPos1,FontPos2-FontPos1+1);
        Result:=Result.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<COLSPAN=',Result.ToUpper);
        if FontPos1=0 then
          Break;
      end;
      inc(i);
    end;
  end;
  if utf8pos('<ROWSPAN=',Result.ToUpper)>0 then
  begin
    FontPos1:=utf8pos('<ROWSPAN=',Result.ToUpper);
    i:=FontPos1;
    while i<= utf8length(Result) do
    begin
      if utf8copy(Result,i,1)='>' then
      begin
        FontPos2:=i;
        tmp1:=utf8copy(Result,FontPos1,FontPos2-FontPos1+1);
        Result:=Result.Replace(tmp1,'',[rfReplaceAll, rfIgnoreCase]);
        FontPos1:=utf8pos('<ROWSPAN=',Result.ToUpper);
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
  (pos('<FONT=',str.ToUpper)>0) or
  (pos('</FONT>',str.ToUpper)>0) or
  (pos('<FONTSIZE=',str.ToUpper)>0) or
  (pos('</FONTSIZE>',str.ToUpper)>0) or
  (pos('<BM',str.ToUpper)>0) or
  (pos('[BM',str.ToUpper)>0) or
  (pos('<COLSPAN=',str.ToUpper)>0) or
  (pos('<ROWSPAN=',str.ToUpper)>0) or
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
      //ColSpan
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='C')
         and (utf8copy(str,i+2,1).ToUpper='O') and (utf8copy(str,i+3,1).ToUpper='L')
         and (utf8copy(str,i+4,1).ToUpper='S') and (utf8copy(str,i+5,1).ToUpper='P')
         and (utf8copy(str,i+6,1).ToUpper='A') and (utf8copy(str,i+7,1).ToUpper='N')
         and (utf8copy(str,i+8,1).ToUpper='=') then
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
      //RowSpan
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='R')
         and (utf8copy(str,i+2,1).ToUpper='O') and (utf8copy(str,i+3,1).ToUpper='W')
         and (utf8copy(str,i+3,1).ToUpper='S') and (utf8copy(str,i+5,1).ToUpper='P')
         and (utf8copy(str,i+6,1).ToUpper='A') and (utf8copy(str,i+7,1).ToUpper='N')
         and (utf8copy(str,i+8,1).ToUpper='=') then
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
      //<Font=xx>
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='F') and (utf8copy(str,i+2,1).ToUpper='O')
         and (utf8copy(str,i+3,1).ToUpper='N') and (utf8copy(str,i+4,1).ToUpper='T')
         and (utf8copy(str,i+5,1).ToUpper='=') then
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
      //</Font>
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='/') and (utf8copy(str,i+2,1).ToUpper='F')
         and (utf8copy(str,i+3,1).ToUpper='O') and (utf8copy(str,i+4,1).ToUpper='N')
         and (utf8copy(str,i+5,1).ToUpper='T') and (utf8copy(str,i+6,1).ToUpper='>') then
      begin
        i:=i+7;
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
    if  FileExists(FBackImageFile) then
    begin
      if FBackgroundImage=nil then
      begin
        FBackgroundImage:=TImage.Create(self);
        FBackgroundImage.Picture.LoadFromFile(FBackImageFile);
      end;
      Buffer.Canvas.StretchDraw(FRect,FBackgroundImage.Picture.Bitmap);
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
  FLineList[i].DispType:='';
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
    FLineList[i].DispType:='LINE';
    str:=str.Replace('[LING]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
  end;
  //双分割线
  if pos('[2LINE]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[2LINE]';
    FLineList[i].DispType:='2LINE';
    str:=str.Replace('[2LING]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
  end;
  //图像
  if pos('[IMG]',str.ToUpper)>0 then
  begin
    textstyle:=textstyle+'[IMG]';
    FLineList[i].DispType:='IMG';
    str:=str.Replace('[IMG]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    if  FileExists(str) then
    begin
      IMG.Picture.LoadFromFile(str);
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
    FLineList[i].DispType:='URL';
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
    FLineList[i].DispType:='COMPONENT';
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
    FLineList[i].DispType:='BOOKMARK1';
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
    FLineList[i].DispType:='BOOKMARK2';
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

procedure TCustomText.Init(Buffer:TBitmap);
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
      if FLineList[i].DispType='URL' then
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
      if FLineList[i].DispType='BOOKMARK1' then
      begin
         FBookMark1[bmsl1].BookMark:=FLineList[i].BookMark1;
         FBookMark1[bmsl1].hs:=i;//所在行数
         inc(bmsl1);
      end
      else
      if FLineList[i].DispType='BOOKMARK2' then
      begin
        FBookMark2[bmsl2].BookMark:=FLineList[i].BookMark2;
        FBookMark2[bmsl2].hs:=i;//所在行数
        inc(bmsl2);
      end;
    end;
  end;
  Buffer.Width := Width;
  Buffer.Height := Height;
  if FOffset = -1 then
    FOffset := Buffer.Height;

  freeandnil(Linetemp);
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
var col,row,js,i,j,dc:integer;
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
              FTable[0,col].Align:=1//居左
              else
              if str=':-:' then
              FTable[0,col].Align:=2//居中
              else
              if str='-:' then
               FTable[0,col].Align:=3//居右
            end
            else
            begin
              if row<=FTablesl[no].row-1 then
              begin
                GetCellInfo(str,MyCell);
                FTable[row,col].str:= MyCell.str;
                FTable[row,col].URL:= MyCell.URL;
                FTable[row,col].DispType:= MyCell.DispType;
                FTable[row,col].bookmarkstr:= MyCell.bookmarkstr;
                FTable[row,col].Align:=MyCell.Align;
                FTable[row,col].Color:=MyCell.Color;
                FTable[row,col].FontStyle:=MyCell.FontStyle;
                FTable[row,col].FontName:=MyCell.FontName;
                FTable[row,col].FontSize:=MyCell.FontSize;
                FTable[row,col].ColSpan:=MyCell.ColSpan;
                FTable[row,col].RowSpan:=MyCell.RowSpan;
                FTable[row,col].Visible:=MyCell.Visible;
                FTable[row,col].ComponentName:=MyCell.ComponentName;
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
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='/') and
    (utf8copy(str,i+2,1).ToUpper='F') and
    (utf8copy(str,i+3,1).ToUpper='O') and
    (utf8copy(str,i+4,1).ToUpper='N') and
    (utf8copy(str,i+5,1).ToUpper='T') and
    (utf8copy(str,i+6,1).ToUpper='>') then
  begin
    Result:='';
    j:=7;
  end
  else
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='F') and
    (utf8copy(str,i+2,1).ToUpper='O') and
    (utf8copy(str,i+3,1).ToUpper='N') and
    (utf8copy(str,i+4,1).ToUpper='T') and
    (utf8copy(str,i+5,1).ToUpper='=') then
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
  //ColSpan=
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='C') and
    (utf8copy(str,i+2,1).ToUpper='O') and
    (utf8copy(str,i+3,1).ToUpper='L') and
    (utf8copy(str,i+4,1).ToUpper='S') and
    (utf8copy(str,i+5,1).ToUpper='P') and
    (utf8copy(str,i+6,1).ToUpper='A') and
    (utf8copy(str,i+7,1).ToUpper='N') and
    (utf8copy(str,i+8,1).ToUpper='=') then
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
  //RowSpan=
  if (Result='<') and (utf8copy(str,i+1,1).ToUpper='R') and
    (utf8copy(str,i+2,1).ToUpper='O') and
    (utf8copy(str,i+3,1).ToUpper='W') and
    (utf8copy(str,i+4,1).ToUpper='S') and
    (utf8copy(str,i+5,1).ToUpper='P') and
    (utf8copy(str,i+6,1).ToUpper='A') and
    (utf8copy(str,i+7,1).ToUpper='N') and
    (utf8copy(str,i+8,1).ToUpper='=') then
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
  (pos('</FONT>',str.ToUpper)>0) or
  (pos('<FONT=',str.ToUpper)>0) or
  (pos('<FONTSIZE=',str.ToUpper)>0) or
  (pos('</FONTSIZE>',str.ToUpper)>0) or
  (pos('<COLSPAN=',str.ToUpper)>0) or
  (pos('<ROWSPAN=',str.ToUpper)>0) or
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
      //<Font=xx>
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='F')
      and (utf8copy(str,i+2,1).ToUpper='O') and (utf8copy(str,i+3,1).ToUpper='N')
      and (utf8copy(str,i+4,1).ToUpper='T') and (utf8copy(str,i+5,1)='=') then
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
        NewFontName:=utf8copy(str,FontPos1+6,FontPos2-FontPos1-6);
        FH1:=Buffer.Canvas.GetTextHeight(NewFontName);
        Buffer.Canvas.font.Name:=NewFontName;//设为新字体
        FH2:=Buffer.Canvas.GetTextHeight(NewFontName);
        y:=y+abs(FH2-FH1) div 2;//修正字体不同时的高度差
      end
      else
      if (DStr='<') and (utf8copy(str,i+1,1).ToUpper='/') and (utf8copy(str,i+2,1).ToUpper='F')
       and (utf8copy(str,i+3,1).ToUpper='O') and (utf8copy(str,i+4,1).ToUpper='N')
       and (utf8copy(str,i+5,1).ToUpper='T') and (utf8copy(str,i+6,1)='>') then
      begin
        i:=i+7;
        Buffer.Canvas.font.Name:=FDefaultFontName;//恢复默认字体
        y:=y-abs(FH2-FH1) div 2;//恢复原来的显示位置
      end
      else
      if (DStr.ToUpper='C') and (utf8copy(str,i+1,1).ToUpper='O')
        and (utf8copy(str,i+2,1).ToUpper='L')
        and (utf8copy(str,i+3,1).ToUpper='S')
        and (utf8copy(str,i+4,1).ToUpper='P')
        and (utf8copy(str,i+5,1).ToUpper='A')
        and (utf8copy(str,i+6,1).ToUpper='N')
        and (utf8copy(str,i+7,1)='=') then
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
      if (DStr.ToUpper='R') and (utf8copy(str,i+1,1).ToUpper='O')
        and (utf8copy(str,i+2,1).ToUpper='W')
        and (utf8copy(str,i+3,1).ToUpper='S')
        and (utf8copy(str,i+4,1).ToUpper='P')
        and (utf8copy(str,i+5,1).ToUpper='A')
        and (utf8copy(str,i+6,1).ToUpper='N')
        and (utf8copy(str,i+7,1)='=') then
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
  TableRowHeigth:integer;
  Span,c:integer;
  th:integer;//文字高度
  row1,col1,oo,oo1:integer;
begin
  if FTablesl<>nil then
  DeleteRecord(index);//删除第2行定义单元格的对齐格式

  row:=FTablesl[Index].row-2;
  col:=FTablesl[Index].col-1;
  TableRowHeigth:=FTablesl[Index].RowHeight;
  Buffer.Canvas.Font.Style:=[fsBold];
  th:= Buffer.Canvas.TextHeight('国');
  h:=round(th*1.5); //表格行高
  if TableRowHeigth>h then h:=TableRowHeigth;
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
      if (FTable[i,j].RowSpan>0) and (FTable[i,j].ColSpan>0) then
      begin
        row1:=FTable[i,j].RowSpan+i-1;
        if row1>row then row1:=row;

        for oo:=i to row1 do
        begin
          col1:=FTable[i,j].ColSpan+j-1;
          if col1>col then col1:=col;
          for oo1:=j to col1 do
          begin
            FTable[oo,oo1].Visible:=false;
          end;
          FTable[i,j].Visible:=true;//将左上角单元格置为true
          FTable[i,j].Width:=colWidth*FTable[i,j].ColSpan;
          FTable[i,j].Height:=h*FTable[i,j].RowSpan;
        end;
      end
      else
      //竖向合并单元格
      if FTable[i,j].RowSpan>0 then
      begin
        FTable[i,j].Height:=h*FTable[i,j].RowSpan;
        row1:=FTable[i,j].RowSpan+i-1;
        if row1>row then row1:=row;
        for oo:=i+1 to row1 do
        begin
          FTable[oo,j].Visible:=false;
        end;
      end
      else
      //横向合并单元格
      if FTable[i,j].ColSpan>0 then
      begin
        FTable[i,j].Width:=colWidth*FTable[i,j].ColSpan;
        col1:=FTable[i,j].ColSpan+j-1;
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
    for j:=0 to col do
    begin
      if FTable[i,j+1].Visible then
      begin
        if (FTable[i,j+1].DispType=0) or (FTable[i,j+1].DispType=2)
           or (FTable[i,j+1].DispType=3) or (FTable[i,j+1].DispType=4) then  //显示文字
        begin
          //设置字体属性
          if FTable[i,j+1].FontName<>'' then
            Buffer.Canvas.Font.Name:=FTable[i,j+1].FontName;
          if FTable[i,j+1].FontStyle=0 then Buffer.Canvas.Font.Style:=[];
          if FTable[i,j+1].FontStyle=1 then Buffer.Canvas.Font.Style:=[fsBold];
          if FTable[i,j+1].FontStyle=2 then Buffer.Canvas.Font.Style:=[fsStrikeOut];
          if FTable[i,j+1].FontStyle=3 then Buffer.Canvas.Font.Style:=[fsItalic];
          if FTable[i,j+1].FontStyle=4 then Buffer.Canvas.Font.Style:=[fsUnderline];
          if FTable[i,j+1].DispType>1 then  Buffer.Canvas.Font.Color:=clBlue //URL,bookmark
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
          if FTable[0,j+1].Align=1 then
            x1:=x0 ;//居左
         if FTable[0,j+1].Align=2 then
            x1:=x0+(colWidth-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j+1].str,colWidth))) div 2; //居中
          if FTable[0,j+1].Align=3 then
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

        if FTable[i,j+1].DispType=4 then //确定URL在表格的位置
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
        if FTable[i,j+1].DispType=2 then //BOOKMARK1
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
        if FTable[i,j+1].DispType=3 then//BOOKMARK2
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
        if FTable[i,j+1].DispType=1 then  //显示图形
        begin
          x0:=FGapX+j*colWidth+1;
          y0:=FOffset + y+FGapY+(i)*h+4;
          if  FileExists(FTable[i,j+1].str) then
          begin
            IMG.Picture.LoadFromFile(FTable[i,j+1].str);
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
    if FLineList[i].DispType='LINE' then
    begin
      Buffer.Canvas.Pen.Color:=FLineList[i].FontColor;
      Buffer.Canvas.Line(FGapX,FOffset + y+FGapY+3,Buffer.Width-FGapX,FOffset + y+FGapY+3);
      y:=y+5;
    end
    else
    if FLineList[i].DispType='2LINE' then
    begin
      Buffer.Canvas.Pen.Color:=FLineList[i].FontColor;
      Buffer.Canvas.Line(FGapX,FOffset + y+FGapY,Buffer.Width-FGapX,FOffset + y+FGapY);
      Buffer.Canvas.Line(FGapX,FOffset + y+FGapY+3,Buffer.Width-FGapX,FOffset + y+FGapY+3);
      y:=y+5;
    end
    else
    if FLineList[i].DispType='IMG' then
    begin
      if  FileExists(FLineList[i].str) then
      begin
        IMG.Picture.LoadFromFile(FLineList[i].str);
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

      if FLineList[i].DispType='BOOKMARK1' then
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
      if FLineList[i].DispType='BOOKMARK2' then
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
  OnPaint := @DrawScrollingText;
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
    if  FileExists(files) then
      FLines.LoadFromFile(files);
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
  FRun:=0;
  FGap:=5;
  FBorder:=true;
  FRowHeight:=30;
  FSelectCol:=-1;
  FSelectRow:=-1;
  Lines.Add(
  '||||||'+#13#10+
  '|:-:|:-:|:-:|:-:|:-:|'+#13#10+
  '||||||'+#13#10+
  '||||||'+#13#10+
  '||||||'+#13#10+
  '||||||'+#13#10+
  '||||||'+#13#10);
  OnPaint := @DisplayTable;
  FOldFontName:=FBuffer.Canvas.Font.Name;
  FConfigFileName:='QFGridPanelConfig.cfg';
end;

destructor TQFGridPanelComponent.Destroy;
begin
  inherited Destroy;
  if FControlsList<>nil then
    FControlsList.Free;
  //FPopupMenu.Free;
end;

procedure TQFGridPanelComponent.SetCellProp;
var
  i,j:integer;
  Index,tmp,err: Integer;
  CellProper :TQFCellProper;
begin
  GetControlsList;
  CellProper := TQFCellProper.Create(Self);

  setlength(CellProper.FTable,FRowCount+1,FColCount);


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
  if FTable[FSelectRow,FSelectCol].Align=0 then FTable[FSelectRow,FSelectCol].Align:=2;
  if FTable[FSelectRow,FSelectCol].Align>0 then
    CellProper.CbxHAlign.ItemIndex:=FTable[FSelectRow,FSelectCol].Align-1;

  if FTable[FSelectRow,FSelectCol].DispType<3 then
    CellProper.CbxCellType.ItemIndex:=FTable[FSelectRow,FSelectCol].DispType
  else
    CellProper.CbxCellType.ItemIndex:=2;//控件
  if (FTable[FSelectRow,FSelectCol].ColSpan<>0) or (FTable[FSelectRow,FSelectCol].RowSpan<>0) then
    CellProper.ChkBoxMerge.Checked:=true
  else
    CellProper.ChkBoxMerge.Checked:=false;
  CellProper.ChkColSizing.Checked:=self.FColSizing;
  CellProper.ChkRowSizing.Checked:=self.FRowSizing;
  CellProper.ColWidthEdit.Text:=FColWidth.ToString;
  CellProper.RowHeightEdit.Text:=FRowHeight.ToString;
  CellProper.EditFontSize.Text:=FTable[FSelectRow,FSelectCol].FontSize.ToString;
  CellProper.LbxFontName.ItemIndex:=CellProper.LbxFontName.Items.IndexOf(FTable[FSelectRow,FSelectCol].FontName);
  CellProper.PanelFontPreview1.Font.Name:=FTable[FSelectRow,FSelectCol].FontName;
  CellProper.LbxFontSize.ItemIndex:=CellProper.LbxFontSize.Items.IndexOf(FTable[FSelectRow,FSelectCol].FontSize.ToString);
  CellProper.PanelFontColor.Color:=FTable[FSelectRow,FSelectCol].FontColor;
  CellProper.DrawBottomLine.Checked:=FTable[FSelectRow,FSelectCol].DrawBottom;
  CellProper.DrawLeftLine.Checked:=FTable[FSelectRow,FSelectCol].DrawLeft;
  CellProper.DrawRightLine.Checked:=FTable[FSelectRow,FSelectCol].DrawRight;
  CellProper.DrawTopLine.Checked:=FTable[FSelectRow,FSelectCol].DrawTop;
  CellProper.LeftLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].LeftLineStyle);
  CellProper.RightLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].RightLineStyle);
  CellProper.BottomLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].BottomLineStyle);
  CellProper.TopLineStyle.ItemIndex:=ord(FTable[FSelectRow,FSelectCol].TopLineStyle);

  CellProper.StringGrid1.RowCount:=FRowCount;
  CellProper.StringGrid1.ColCount:=FColCount;

  for i:=0 to FRowCount-1 do
  begin
    CellProper.StringGrid1.RowHeights[i]:=FRowHeight;
    for j:=1 to FColCount do
    begin
      CellProper.FTable[i,j-1]:=FTable[i,j-1];
      if FTable[i,j].ComponentName<>'' then
        CellProper.StringGrid1.cells[j-1,i]:=FTable[i,j].ComponentName
      else
        CellProper.StringGrid1.cells[j-1,i]:=FTable[i,j].str;
      CellProper.StringGrid1.ColWidths[j-1]:=FColWidth;
    end;
  end;
  CellProper.ShowModal;
  ////////////////////////////////
  //单元格设置后
  if QFCellProperReturn then
  begin
    if FCellLineColor<>CellProper.LineColor.Color then
    begin
      FCellLineColor:=CellProper.LineColor.Color;
      DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
    end;
    if FCellLineStyle<>TFPPenStyle(CellProper.CbxLineStyle.ItemIndex) then
    begin
      FCellLineStyle:=TFPPenStyle(CellProper.CbxLineStyle.ItemIndex);
    end;

    if CellProper.CbxHAlign.ItemIndex>=0 then
      FTable[FSelectRow,FSelectCol].Align:=CellProper.CbxHAlign.ItemIndex+1;

    if (not CellProper.ChkBoxMerge.Checked) and ((FTable[FSelectRow,FSelectCol].ColSpan<>0) or (FTable[FSelectRow,FSelectCol].RowSpan<>0))  then
    begin
      FTable[FSelectRow,FSelectCol].RowSpan:=0;
      FTable[FSelectRow,FSelectCol].ColSpan:=0;
      FTable[FSelectRow,FSelectCol].Width:= FColWidth;
      FTable[FSelectRow,FSelectCol].Height:= FRowHeight;
    end;
    FColSizing:=CellProper.ChkColSizing.Checked;
    FRowSizing:=CellProper.ChkRowSizing.Checked;

    if CellProper.LbxFontName.ItemIndex>=0 then
      FTable[FSelectRow,FSelectCol].FontName:=CellProper.LbxFontName.Items.ValueFromIndex[CellProper.LbxFontName.ItemIndex];
    if CellProper.LbxFontSize.itemindex>=0 then
    begin
      val(CellProper.LbxFontSize.Items.ValueFromIndex[CellProper.LbxFontSize.itemindex],tmp,err);
      FTable[FSelectRow,FSelectCol].FontSize:=tmp;
    end;
    FTable[FSelectRow,FSelectCol].FontColor:=CellProper.PanelFontColor.Color;

    val(CellProper.ColWidthEdit.Text,tmp,err);
    FColWidth:=tmp;
    val(CellProper.RowHeightEdit.Text,tmp,err);
    FRowHeight:=tmp;

    FTable[FSelectRow,FSelectCol].DrawBottom:=CellProper.DrawBottomLine.Checked;
    FTable[FSelectRow,FSelectCol].DrawLeft:=CellProper.DrawLeftLine.Checked;
    FTable[FSelectRow,FSelectCol].DrawRight:=CellProper.DrawRightLine.Checked;
    FTable[FSelectRow,FSelectCol].DrawTop:=CellProper.DrawTopLine.Checked;
    FTable[FSelectRow,FSelectCol].LeftLineStyle:=TFPPenStyle(CellProper.LeftLineStyle.ItemIndex);
    FTable[FSelectRow,FSelectCol].RightLineStyle:=TFPPenStyle(CellProper.RightLineStyle.ItemIndex);
    FTable[FSelectRow,FSelectCol].BottomLineStyle:=TFPPenStyle(CellProper.BottomLineStyle.ItemIndex);
    FTable[FSelectRow,FSelectCol].TopLineStyle:=TFPPenStyle(CellProper.TopLineStyle.ItemIndex);

    val(CellProper.ColEdit.Text,tmp,err);
    if tmp<>FColCount then
      FColCount:=tmp;
    val(CellProper.RowEdit.Text,tmp,err);
    if tmp<>FRowCount then
      FRowCount:=tmp;


    for i:=0 to FRowCount-1 do
    begin
      for j:=1 to FColCount do
      begin
        FTable[i,j]:=CellProper.FTable[i,j-1];
        FTable[i,j].str:=CellProper.StringGrid1.cells[j-1,i];
      end;
    end;

    Invalidate;

  end;
  CellProper.Free;
end;

procedure TQFGridPanelComponent.MenuItemClick(Sender: TObject);
begin
  //selectcell:= FOnSelectCell;
  //FOnSelectCell:=nil;
  case TStMenuItemTag(TMenuItem(Sender).Tag) of
    //mtCut :
    //  CutCells(TRect(Selection));
    //mtCopy :
    //  CopyCells(TRect(Selection));
    //mtPaste :
    //  PasteCells(TPoint(FCurrent));
    //mtInsertCol :
    //  InsertCol(TRect(Selection));
    //mtInsertRow :
    //  InsertRow(TRect(Selection));
    //mtDeleteCol :
    //   if Selection.Left>2 then   DeleteCol(TRect(Selection));
    //mtDeleteRow :
    // DeleteRow(TRect(Selection));
    //mtClearCells :
    //  ClearCells(TRect(Selection));
    mtSetCellProp :
      SetCellProp;
  end;
  //FOnSelectCell:=selectcell;
end;

// 初始化弹出式菜单
procedure TQFGridPanelComponent.InitPopupMenu;
var
  AMenuItem, BMenuItem: TMenuItem;
begin
  AMenuItem := TMenuItem.Create(FPOpupMenu);
  AMenuItem.Caption := '剪切(&T)';
  AMenuItem.Tag := Ord(mtCut);
  AMenuItem.OnClick := @MenuItemClick;
  FPOpupMenu.Items.Add(AMenuItem);

  AMenuItem := TMenuItem.Create(FPOpupMenu);
  AMenuItem.Caption := '复制(&C)';
  AMenuItem.Tag := Ord(mtCopy);
  AMenuItem.OnClick := @MenuItemClick;
  FPOpupMenu.Items.Add(AMenuItem);

  AMenuItem := TMenuItem.Create(FPOpupMenu);
  AMenuItem.Caption := '粘贴(&P)';
  AMenuItem.Tag := Ord(mtPaste);
  AMenuItem.OnClick := @MenuItemClick;
  FPOpupMenu.Items.Add(AMenuItem);

  AMenuItem := TMenuItem.Create(FPOpupMenu);
  AMenuItem.Caption := '-';
  FPOpupMenu.Items.Add(AMenuItem);
{
  AMenuItem := TMenuItem.Create(FPOpupMenu);
  AMenuItem.Caption := '插入';
  FPOpupMenu.Items.Add(AMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := '插入整行';
    BMenuItem.Tag := Ord(mtInsertRow);
    BMenuItem.OnClick := @MenuItemClick;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := '插入整列';
    BMenuItem.Tag := Ord(mtInsertCol);
    BMenuItem.OnClick := @MenuItemClick;
    AMenuItem.Add(BMenuItem);

    AMenuItem := TMenuItem.Create(FPOpupMenu);
    AMenuItem.Caption := '删除';
    FPOpupMenu.Items.Add(AMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := '删除整行';
    BMenuItem.Tag := Ord(mtDeleteRow);
    BMenuItem.OnClick := @MenuItemClick;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := '删除整列';
    BMenuItem.Tag := Ord(mtDeleteCol);
    BMenuItem.OnClick := @MenuItemClick;
    AMenuItem.Add(BMenuItem);
}
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
begin
  inherited DoOnChangeBounds;
  FRun:=0;
  FOldR.Left:=0;
  FOldR.Top:=0;
  FOldR.Width:=0;
  FOldR.Height:=0;
  //Init(FBuffer);
  //if FTablesl<>nil then
  //  GetTableInfo(0);
  //Tableiniti(FBuffer,FColWidth,FRowHeight,0,0);
  //DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
end;

procedure TQFGridPanelComponent.GetControlsList;
var
  i:integer;
begin
  FControlsList:=TStringlist.Create;
  for i:=0 to self.Parent.ControlCount-1 do
  begin
    if  UpperCase('QFGridPanelComponent')<>UpperCase(copy(self.Parent.Controls[i].Name,1,20)) then
    begin
      FControlsList.Add(self.Parent.Controls[i].Name);
    end;
  end;
end;

procedure TQFGridPanelComponent.LoadFile(filename:string);
var
  jsonFile: TStringList;
begin
  try
    jsonFile:=TStringList.Create;
    if trim(filename)='' then
      filename:=FConfigFileName;

    if  FileExists(filename) then
    begin
      jsonFile.LoadFromFile(filename);
      loadjson(jsonFile.Text);
    end;
  finally
    jsonFile.Free;
  end;
end;

procedure TQFGridPanelComponent.LoadFile;
var
  jsonFile: TStringList;
begin
  try
    jsonFile:=TStringList.Create;
    if  FileExists(FConfigFileName) then
    begin
      jsonFile.LoadFromFile(FConfigFileName);
      loadjson(jsonFile.Text);
    end;
  finally
    jsonFile.Free;
  end;
end;

procedure TQFGridPanelComponent.SaveFile(filename:string);
begin
  if trim(filename)='' then
    filename:=FConfigFileName;
  savejson(filename);
end;

procedure TQFGridPanelComponent.SaveFile;
begin
  savejson(FConfigFileName);
end;

procedure TQFGridPanelComponent.Refresh;
begin
  FRun:=0;
  Init(FBuffer);
  if FTablesl<>nil then
    GetTableInfo(0);
  Tableiniti(FBuffer,FColWidth,FRowHeight,0,0);
  DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
  //Canvas.Draw(0,0,FBuffer)
end;

procedure TQFGridPanelComponent.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    FRun:=0;
    FLines.Assign(AValue);
    Init(FBuffer);
    if FTablesl<>nil then
      GetTableInfo(0);
    Tableiniti(FBuffer,FColWidth,FRowHeight,0,0);
    DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
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
    Init(FBuffer);
    if FTablesl<>nil then
      GetTableInfo(0);
    Tableiniti(FBuffer,FColWidth,FRowHeight,0,0);
  end;
  DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
  //Canvas.Draw(0,0,FBuffer)
end;

procedure TQFGridPanelComponent.Tableiniti(Buffer: TBitmap;colWidth,h,Index,y:integer);
var
  i,j,w1,h1,row,col,v:integer;
  i1:integer;
  hg:integer;
  k:integer;
  x0,y0,x1,y1:integer;
  TableRowHeigth:integer;
  Span,c:integer;
  th:integer;//文字高度
  TabStops:integer;
  Control: TControl;
  row1,col1:integer;
  kk,oo,oo1,oo2:integer;
  str:string;
begin
  index:=0;
  DeleteRecord(index);//删除第2行定义单元格的对齐格式
  row:=FTablesl[Index].row-2;
  col:=FTablesl[Index].col-1;
  FRowCount:=row;
  FColCount:=col;
  TableRowHeigth:=FTablesl[Index].RowHeight;
  Buffer.Canvas.Font.Style:=[fsBold];
  th:= Buffer.Canvas.TextHeight('国');
  if (h=0) and (FRowHeight=0) then
    h:=round(th*1.5); //表格行高
  if FRowHeight>h then h:= FRowHeight; //FRowHeight--设定的行高
  if FRowHeight<=1 then FRowHeight:=h;

  if (colWidth=0) and (FColWidth=0) then
    colWidth:=(Buffer.Width) div col; //单元格宽
  if FColWidth=0 then
    FColWidth:=colWidth;

  Buffer.Canvas.Pen.Color:=FCellLineColor;//黑色画笔

  //重新计算合并后单元格的width和height
  for i:=0 to row do
  begin
    for j:=0 to col do
    begin
      FTable[i,j].Height:=h;
      FTable[i,j].Width:=colWidth;
      FTable[i,j].DrawTop:=true;
      FTable[i,j].DrawLeft:=true;
      FTable[i,j].DrawBottom:=true;
      FTable[i,j].DrawRight:=true;
      FTable[i,j].LineStyle:=FCellLineStyle;
      //FTable[i,j].TopLineStyle:=FCellLineStyle;
      //FTable[i,j].LeftLineStyle:=FCellLineStyle;
      //FTable[i,j].BottomLineStyle:=FCellLineStyle;
      //FTable[i,j].RightLineStyle:=FCellLineStyle;

      //竖向(row)合并单元格
      if (FTable[i,j].RowSpan>0) and (FTable[i,j].ColSpan>0) then
      begin
        row1:=FTable[i,j].RowSpan+i-1;
        if row1>row then row1:=row;

        for oo:=i to row1 do
        begin
          col1:=FTable[i,j].ColSpan+j-1;
          if col1>col then col1:=col;
          for oo1:=j to col1 do
          begin
            FTable[oo,oo1].Visible:=false;
          end;
          FTable[i,j].Visible:=true;//将左上角单元格置为true
          FTable[i,j].Width:=colWidth*FTable[i,j].ColSpan;
          FTable[i,j].Height:=h*FTable[i,j].RowSpan;
        end;
      end
      else
      //竖向(row)合并单元格
      if FTable[i,j].RowSpan>0 then
      begin
        FTable[i,j].Height:=h*FTable[i,j].RowSpan;
        row1:=FTable[i,j].RowSpan+i-1;
        if row1>row then row1:=row;
        for oo:=i+1 to row1 do
        begin
          FTable[oo,j].Visible:=false;
        end;
      end
      else
      //横向(col)合并单元格
      if FTable[i,j].ColSpan>0 then
      begin
        FTable[i,j].Width:=colWidth*FTable[i,j].ColSpan;
        col1:=FTable[i,j].ColSpan+j-1;
        if col1>col then col1:=col;
        for oo:=j+1 to col1 do
        begin
          FTable[i,oo].Visible:=false;
        end;
      end;
    end;
  end;
end;

function TQFGridPanelComponent.FindChildControls(str:string):TControl;
var kk,i:integer;
begin
  Result:=nil;
  kk:=self.Parent.ControlCount;
  for i:=0 to kk-1 do
  begin
    if UpperCase(self.Parent.Controls[i].Name) =str.ToUpper then
    begin
      Result:=self.Parent.Controls[i];
      break;
    end;
  end;
end;

function TQFGridPanelComponent.DrawTable(Buffer: TBitmap;colWidth,h,Index,y:integer):integer;
var
  i,j,w1,h1,row,col,v:integer;
  i1:integer;
  hg:integer;
  k:integer;
  x0,y0,x1,y1:integer;
  TableRowHeigth:integer;
  Span,c:integer;
  th:integer;//文字高度
  TabStops:integer;
  Control: TControl;
  row1,col1:integer;
  kk,oo,oo1,oo2:integer;
  str:string;
begin
  Index:=0;
  y:=0;
  if FTablesl<>nil then
  begin
    BackgroundRefresh(Buffer); //刷新背景
    TableRowHeigth:=FTablesl[Index].RowHeight;
    Buffer.Canvas.Font.Style:=[fsBold];

    th:= Buffer.Canvas.TextHeight('国');
    if (h=0) and (FRowHeight=0) then
      h:=round(th*1.5); //表格行高
    if FRowHeight>h then h:= FRowHeight; //FRowHeight--设定的行高
    if FRowHeight=0 then FRowHeight:=h;

    if (colWidth=0) and (FColWidth=0) then
      colWidth:=(Buffer.Width) div col; //单元格宽
    if FColWidth=0 then
      FColWidth:=colWidth;

    Buffer.Canvas.Pen.Color:=FCellLineColor;  //边框颜色
    Buffer.Canvas.Pen.Style:=FCellLineStyle;  //边框线条样式

    //画单元格
    hg:=0;
    x0:=0;
    for i:=0 to FRowCount do
    begin
      x0:=0;
      for j:=1 to FColCount do
      begin
        if FTable[i,j].Visible then
        begin
          if j>1 then
            x0:=x0+FTable[i,j-1].Width;

          if FTable[i,j-1].Height>h then
            y0:=i*h
          else
            y0:=i*FTable[i,j-1].Height;

          FTable[i,j].x:=x0;
          FTable[i,j].y:=y0;

          x1:=x0+FTable[i,j].Width;
          y1:=y0+FTable[i,j].Height;

          if x0>=Buffer.Width then
            x0:=Buffer.Width-1;
          if x1>=Buffer.Width then
            x1:=Buffer.Width-1;
          if FTable[i,j].DrawTop then
          begin
            if FTable[i,j].TopLineStyle<>FCellLineStyle then
              Buffer.Canvas.Pen.Style:=FTable[i,j].TopLineStyle
            else
              Buffer.Canvas.Pen.Style:=FCellLineStyle;
            Buffer.Canvas.Line(x0,y0,x1,y0);//顶横线
          end;
          if FTable[i,j].DrawRight then
          begin
            if FTable[i,j].RightLineStyle<>FCellLineStyle then
              Buffer.Canvas.Pen.Style:=FTable[i,j].RightLineStyle
            else
              Buffer.Canvas.Pen.Style:=FCellLineStyle;
            Buffer.Canvas.Line(x1,y0,x1,y1);//右竖线
          end;
          if FTable[i,j].DrawBottom then
          begin
            if FTable[i,j].BottomLineStyle<>FCellLineStyle then
              Buffer.Canvas.Pen.Style:=FTable[i,j].BottomLineStyle
            else
              Buffer.Canvas.Pen.Style:=FCellLineStyle;
            Buffer.Canvas.Line(x1,y1,x0,y1);//底横线
          end;
          if FTable[i,j].DrawLeft then
          begin
            if FTable[i,j].LeftLineStyle<>FCellLineStyle then
              Buffer.Canvas.Pen.Style:=FTable[i,j].LeftLineStyle
            else
              Buffer.Canvas.Pen.Style:=FCellLineStyle;
            Buffer.Canvas.Line(x0,y1,x0,y0);//左竖线
          end;
        end;
      end;
    end;
    FTableWidth:=x1;
    FTableHeight:=y1;

    //开始画表格最外框边框
    Buffer.Canvas.Pen.Color:=clBlack;//黑色画笔
    Buffer.Canvas.Pen.Style:=psSolid;
    if FBorder then
    begin
      Buffer.Canvas.Pen.Width:=2;
      Buffer.Canvas.MoveTo(1,1);
      if FColWidth*FColCount>=Buffer.Canvas.Width then
      begin
        Buffer.Canvas.LineTo(Buffer.Canvas.Width-1,1);  //顶
        Buffer.Canvas.LineTo(Buffer.Canvas.Width-1,h*(FRowCount+1));//右;
      end
      else
      begin
        Buffer.Canvas.LineTo(FColWidth*FColCount-1,1);
        Buffer.Canvas.LineTo(FColWidth*FColCount-1,h*(FRowCount+1));
      end;
      Buffer.Canvas.LineTo(1,h*(FRowCount+1)); //底
      Buffer.Canvas.LineTo(1,1);//左
      Buffer.Canvas.Pen.Width:=1;
    end;
    //结束画表格边框

    //绘表格内容（文字/图像/控件)
    Buffer.Canvas.Brush.Style := bsClear;//透明文字
    TabStops:=0;
    x0:=0;
    for i:=0 to FRowCount do
    begin
      x0:=0;
      for j:=1 to FColCount do
      begin
        if FTable[i,j].Visible then
        begin
          if (FTable[i,j].DispType=0) or (FTable[i,j+1].DispType=2)
             or (FTable[i,j].DispType=3) or (FTable[i,j+1].DispType=4) then  //显示文字
          begin
            //设置字体属性
            if FTable[i,j].FontName<>'' then
              Buffer.Canvas.Font.Name:=FTable[i,j].FontName
            else
              Buffer.Canvas.Font.Name:=FOldFontName;
            if FTable[i,j].FontStyle=0 then Buffer.Canvas.Font.Style:=[];
            if FTable[i,j].FontStyle=1 then Buffer.Canvas.Font.Style:=[fsBold];
            if FTable[i,j].FontStyle=2 then Buffer.Canvas.Font.Style:=[fsStrikeOut];
            if FTable[i,j].FontStyle=3 then Buffer.Canvas.Font.Style:=[fsItalic];
            if FTable[i,j].FontStyle=4 then Buffer.Canvas.Font.Style:=[fsUnderline];
            if (FTable[i,j].DispType>1) and (FTable[i,j].DispType<5) then  Buffer.Canvas.Font.Color:=clBlue //URL,bookmark
            else Buffer.Canvas.Font.Color:=FTable[i,j].Color;
            Buffer.Canvas.Font.Size:=FTable[i,j].FontSize;
            th:= Buffer.Canvas.TextHeight('国');
            //设置字体属性

            x0:=FTable[i,j].x;

            h1:=h;
            w1:=FTable[i,j].Width;

            if FTable[i,j].Height>0 then
               h:=FTable[i,j].Height;

            x1:=x0; //居左
            if FTable[0,j].Align=1 then
              x1:=x0 ;//居左
            if FTable[0,j].Align=2 then
              x1:=x0+(FTable[i,j].Width-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j].str,FTable[i,j].Width))) div 2; //居中
            if FTable[0,j].Align=3 then
               x1:=x0+(FTable[i,j].Width-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j].str,FTable[i,j].Width)))-5; //居右

            if i=0 then
            begin
              y0:=y+i*FTable[i,j].Height+abs(FTable[i,j].Height- th) div 2;//垂直居中
              Buffer.Canvas.Font.Color:=FTable[i,j+1].Color;
              DisplayChar(Buffer,x1+2, y0,TruncationStr(Buffer,FTable[i,j].str,FTable[i,j].Width));//截断超过单元格宽度的字符串
            end
            else
            begin
               if FTable[i,j].Height>FRowHeight then
                 y0:=y + i* FRowHeight + abs (FTable[i,j].Height-th) div 2//垂直居中
               else
                 y0:=y + i* FTable[i,j].Height + abs(FTable[i,j].Height- th) div 2;//垂直居中
               DisplayChar(Buffer,x1+2, y0,TruncationStr(Buffer,FTable[i,j].str,FTable[i,j].Width));
            end;
            h:=h1;
          end;
          if (FTable[i,j].DispType=5) and (FRun=0) then  //控件
          begin
            Control:=FindChildControls(FTable[i,j].ComponentName);//查找控件
            if Control<>nil then
            begin
              Control.Width:=FTable[i,j].Width-FGap*2;
              Control.Height:= FTable[i,j].Height-FGap*2;
              y1:=(abs(Control.Height-FTable[i,j].Height) div 2)-FGap;
              Control.Left:=FTable[i,j].x+FGap+self.Left;
              Control.Top:=FTable[i,j].y+FGap+self.Top+y1;//垂直居中显示控件
              if (Control is TEdit) or (Control is TDBEdit) or
                 (Control is TMemo) or
                 (Control is TDBMemo) or
                 (Control is TComboBox) or (Control is TDBComboBox) then
              begin
                FOldEditFocusColor:=TEdit(Control).Color;
                FOldEditFontFocusColor:=TEdit(Control).Font.Color;
                TEdit(Control).BorderStyle:=bsNone;
                TEdit(Control).TabOrder:= TabStops;
                TEdit(Control).OnEnter:=@EditEnter;
                TEdit(Control).OnExit:=@EditExit;
              end;
              inc(TabStops);
            end;
          end;
          if FTable[i,j].DispType=1 then  //显示图形
          begin
            x0:=FTable[i,j].x+1;
            y0:=FTable[i,j].y+1;

            if  FileExists(FTable[i,j].str) then
            begin
              IMG.Picture.LoadFromFile(FTable[i,j].str);
              //设置图像显示位置及尺寸（单元格大小）
              FRect.Top:=y0+1;
              FRect.Left:=x0+1;
              if FTable[i,j].Width<>0 then
                FRect.Width:=FTable[i,j].Width-4
              else
                FRect.Width:=colWidth-3;
              if FTable[i,j].Height<>0 then
                FRect.Height:=FTable[i,j].Height-4
              else
                FRect.Height:=h-3;
              Buffer.Canvas.StretchDraw(FRect,img.Picture.Bitmap);
            end
            else
            begin
              //没找到图像文件
              DisplayChar(Buffer,x0+2, y0,TruncationStr(Buffer,'['+ExtractFileName(FTable[i,j].str+']'),colWidth));
            end;
          end;
        end;
      end;
    end;
    FRun:=1;
    y:=y+(FRowCount)*h+5;
    Result:=y;
    Canvas.Draw(0,0,Buffer);
  end;
end;

procedure TQFGridPanelComponent.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    // 处理左键按下
    FisLeftButtonDown := True;
    FinitialXY.X := X;
    FinitialXY.Y := Y;
    if FTable[FSelectRow,FSelectCol].DispType=5 then
      DrawRect(FCurrentR,clred,1,FSelectRow,FSelectCol)
    else
      DrawRect(FCurrentR,clBlue,1,FSelectRow,FSelectCol);
  end;
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

procedure TQFGridPanelComponent.DrawRect(rect:TRect;colors:TColor;Linewidth,x,y:integer;RepaintRect:byte=0);
var
  CellControl:TControl;
  linestyle:TFPPenStyle;

  function isCellComponent(Rect:TRect;out Control: TControl):Boolean;
  var
    i,j,x0,x1,y0,y1:integer;
  begin
    Result:=false;
    for i:=0 to FRowCount do
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
            if FTable[i,j].DispType=5 then
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
    Canvas.FillRect(rect);
    if CellControl<>nil then
    begin
      if (CellControl is TEdit) or
         (CellControl is TDBEdit) or
         (CellControl is TMemo) or
         (CellControl is TDBMemo) or
         (CellControl is TComboBox) or
         (CellControl is TDBComboBox) then
      begin
        FOldEditFocusColor:=TEdit(CellControl).Color;
        FOldEditFontFocusColor:=TEdit(CellControl).Font.Color;
        TEdit(CellControl).Color:=Canvas.Brush.Color;
      end;
    end;
    Canvas.Brush.Color:=FoldEditFocusColor;
  end;
  Canvas.Pen.Color:=colors;
  Canvas.MoveTo(rect.Left,rect.Top);
  if rect.Left+rect.Width>=FBuffer.Width then
  rect.Width:=rect.Width-1;
  //顶
  if (rect.Top=0) and (FBorder) then
  begin
    Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=3;
  end
  else
  begin
    if RepaintRect=1 then//重绘单元格
    begin
      Canvas.Pen.Style:=FTable[x,y].TopLineStyle;
      Canvas.Pen.Color:=FCellLineColor;
    end
    else
    begin
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
    end;
    Canvas.Pen.Width:=1;
  end;
  Canvas.LineTo(rect.Left+rect.Width,rect.Top);  //顶
  //右;
  if (rect.Left+rect.Width=FTableWidth)  and (FBorder) then
  begin
    Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=2;
  end
  else
  begin
    if RepaintRect=1 then
    begin
      Canvas.Pen.Style:=FTable[x,y].RightLineStyle;
      Canvas.Pen.Color:=FCellLineColor;
    end
    else
    begin
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
    end;
    Canvas.Pen.Width:=1;
  end;
  Canvas.LineTo(rect.Left+rect.Width,rect.Top+rect.Height);//右;
  //底
  if (rect.Top+rect.Height=FTableHeight) and (FBorder)  then
  begin
    Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=2;
  end
  else
  begin
    if RepaintRect=1 then
    begin
      Canvas.Pen.Style:=FTable[x,y].BottomLineStyle;
      Canvas.Pen.Color:=FCellLineColor;
    end
    else
    begin
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
    end;
    Canvas.Pen.Width:=1;
  end;
  Canvas.LineTo(rect.Left,rect.Top+rect.Height); //底
  //左
  if (rect.Left=0) and (FBorder)  then
  begin
    Canvas.Pen.Style:=psSolid;
    if Canvas.Pen.Color=FCellLineColor then
      Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=2;
  end
  else
  begin
    if RepaintRect=1 then
    begin
      Canvas.Pen.Style:=FTable[x,y].LeftLineStyle;
      Canvas.Pen.Color:=FCellLineColor;
    end
    else
    begin
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Color:=colors;
    end;
    Canvas.Pen.Width:=1;
  end;
  Canvas.LineTo(rect.Left,rect.Top);//左
  Canvas.Pen.Width:=1;
  //Canvas.Draw(0,0,FBuffer);
end;

procedure TQFGridPanelComponent.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var movedX:integer;
  movedY:integer;
  i,j:integer;
begin
  if FisLeftButtonDown then  //按下鼠标左键
  begin
    if FResultCursor=crVSplit then //调整单元格高度
    begin
      FOldR.Left:=0;
      FOldR.Top:=0;
      FOldR.Width:=0;
      FOldR.Height:=0;
      movedY := Y - FinitialXY.Y; // 计算Y轴上的移动距离
      FRowHeight:=FRowHeight+movedY;
      FRun:=0;
      //Init(FBuffer);
      //if FTablesl<>nil then
      //  GetTableInfo(0);
      DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
      //Canvas.Draw(0,0,FBuffer)
    end;
    if FResultCursor=crHSplit then //调整单元格宽度
    begin
      FOldR.Left:=0;
      FOldR.Top:=0;
      FOldR.Width:=0;
      FOldR.Height:=0;
      movedX := X - FinitialXY.X; // 计算Y轴的移动距离
      if ssCtrl in Shift then
      begin
        for i:=0 to FRowCount do
        begin
          if movedX>0 then
          begin
            if FTable[i,FMoveCol-1].x+FTable[i,FMoveCol-1].Width+movedX<FBuffer.Width then
              FTable[i,FMoveCol-1].Width:=FTable[i,FMoveCol-1].Width+movedX;
            if FTable[i,FMoveCol].x+FTable[i,FMoveCol].Width-movedX<FBuffer.Width then
              FTable[i,FMoveCol].Width:=FTable[i,FMoveCol].Width-movedX;
            FTable[i,FMoveCol].x:=FTable[i,FMoveCol-1].x+movedX;
          end
          else
          begin
            if FTable[i,FMoveCol-1].x+FTable[i,FMoveCol-1].Width+movedX<FBuffer.Width then
              FTable[i,FMoveCol-1].Width:=FTable[i,FMoveCol-1].Width+movedX;
            if FTable[i,FMoveCol].x+ FTable[i,FMoveCol].Width+abs(movedX)<FBuffer.Width then
              FTable[i,FMoveCol].Width:=FTable[i,FMoveCol].Width+abs(movedX);
            FTable[i,FMoveCol].x:=FTable[i,FMoveCol].x-abs(movedX);
          end;
        end;
      end
      else
      begin
        if movedX>0 then
        begin
          if FTable[FMoveRow,FMoveCol-1].x+FTable[FMoveRow,FMoveCol-1].Width+movedX<FBuffer.Width then
            FTable[FMoveRow,FMoveCol-1].Width:=FTable[FMoveRow,FMoveCol-1].Width+movedX;

          if FTable[FMoveRow,FMoveCol].x+FTable[FMoveRow,FMoveCol].Width-movedX=FBuffer.Width then
            FTable[FMoveRow,FMoveCol].Width:=FTable[FMoveRow,FMoveCol].Width+movedX
          else
          if FTable[FMoveRow,FMoveCol].x+FTable[FMoveRow,FMoveCol].Width-movedX<FBuffer.Width then
            FTable[FMoveRow,FMoveCol].Width:=FTable[FMoveRow,FMoveCol].Width-movedX;

          FTable[FMoveRow,FMoveCol].x:=FTable[FMoveRow,FMoveCol-1].x+movedX;
        end
        else
        begin
          if FTable[FMoveRow,FMoveCol].x+ FTable[FMoveRow,FMoveCol].Width=FBuffer.Width then
          begin
            FTable[FMoveRow,FMoveCol-1].Width:=FTable[FMoveRow,FMoveCol-1].Width-abs(movedX);
            FTable[FMoveRow,FMoveCol].Width:=FTable[FMoveRow,FMoveCol].Width+abs(movedX);
            FTable[FMoveRow,FMoveCol].x:=FTable[FMoveRow,FMoveCol].x-abs(movedX);
          end
          else
          begin
            if FTable[FMoveRow,FMoveCol-1].x+FTable[FMoveRow,FMoveCol-1].Width+movedX<FBuffer.Width then
              FTable[FMoveRow,FMoveCol-1].Width:=FTable[FMoveRow,FMoveCol-1].Width+movedX;

            if FTable[FMoveRow,FMoveCol].x+ FTable[FMoveRow,FMoveCol].Width+abs(movedX)<FBuffer.Width then
              FTable[FMoveRow,FMoveCol].Width:=FTable[FMoveRow,FMoveCol].Width+abs(movedX);
            FTable[FMoveRow,FMoveCol].x:=FTable[FMoveRow,FMoveCol].x-abs(movedX);
          end;
        end;
      end;
      FRun:=0;
      DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
      Canvas.Draw(0,0,FBuffer)
    end;
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
    if FTable[FSelectRow,FSelectCol].DispType=5 then
      DrawRect(FCurrentR,clred,1,FSelectRow,FSelectCol)
    else
      DrawRect(FCurrentR,clBlue,1,FSelectRow,FSelectCol);
     FPopupMenu.PopUp(ControlToScreenX(self)+x,ControlToScreenY(self)+y);
  end;
  inherited MouseUP(Button,Shift, X, Y);
end;

procedure TQFGridPanelComponent.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i,j:integer;

  function isCell(x,y:integer;out Rect:TRect):Boolean;
  var
    i,j,x0,x1,y0,y1:integer;
  begin
    Result:=false;
    Rect.Left:=0;
    Rect.Top:=0;
    Rect.Width:=0;
    Rect.Height:=0;
    for i:=0 to FRowCount do
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

  function isLine(x,y:integer;out i0,j0:integer):TCursor;
  var
    i,j,x0,x1,y0,y1:integer;
  begin
    Cursor:=crDefault;
    Result:=Cursor;
    for i:=0 to FRowCount do
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

  if isCell(x,y,FCurrentR) then
  begin
    DrawRect(FoldR,FCellLineColor,1,FSelectRow,FSelectCol,1);
    FOldR:=FCurrentR;
    if FTable[FSelectRow,FSelectCol].DispType=5 then
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

procedure TQFGridPanelComponent.LoadJSON(jsonstr:string);
var
  jsonRoot: TJSONObject;
  jData : TJSONData;
  jItem,kItem,litem : TJSONData;
  i,j,k,no: Integer;
  Versions,str:string;
  //object_name: String;
begin
  jData := GetJSON(utf8toansi(jsonstr));
  jsonRoot:=TJSONObject(jData);

  if jsonRoot.get('TQFGridPanelComponent')='TQFGridPanelComponent配置' then
  begin
    Versions:= jsonRoot.get('Version');
    for i := 0 to jData.Count - 1 do
    begin
      jItem := jData.Items[i];

      no:=0;
      for j := 0 to jItem.Count - 1 do  //row
      begin
        kItem:=jItem.Items[j];

        str := TJSONObject(jItem).Names[j];
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
        if str.ToUpper='ROW0' then no:=j;

        for k := 0 to kItem.Count - 1 do //col
        begin
          lItem:=kItem.Items[k];

          FTable[j-no,k].x:=TJSONObject(lItem.Items[0]).Get('x');
          FTable[j-no,k].y:=TJSONObject(lItem.Items[0]).Get('y');
          FTable[j-no,k].Width:=TJSONObject(lItem.Items[0]).Get('Width');
          FTable[j-no,k].Height:=TJSONObject(lItem.Items[0]).Get('Height');
          FTable[j-no,k].ColSpan:=TJSONObject(lItem.Items[0]).Get('ColSpan');
          FTable[j-no,k].RowSpan:=TJSONObject(lItem.Items[0]).Get('RowSpan');
          FTable[j-no,k].DispType:=TJSONObject(lItem.Items[0]).Get('DispType');
          FTable[j-no,k].str:=TJSONObject(lItem.Items[0]).Get('str');
          FTable[j-no,k].Color:=TJSONObject(lItem.Items[0]).Get('Color');
          FTable[j-no,k].Align:=TJSONObject(lItem.Items[0]).Get('Align');
          FTable[j-no,k].FontName:=TJSONObject(lItem.Items[0]).Get('FontName');
          FTable[j-no,k].FontSize:=TJSONObject(lItem.Items[0]).Get('FontSize');
          FTable[j-no,k].FontStyle:=TJSONObject(lItem.Items[0]).Get('FontStyle');
          FTable[j-no,k].FontColor:=TJSONObject(lItem.Items[0]).Get('FontColor');
          FTable[j-no,k].ComponentType:=TJSONObject(lItem.Items[0]).Get('ComponentType');
          FTable[j-no,k].ComponentName:=TJSONObject(lItem.Items[0]).Get('FontSComponentNametyle');
          //FTable[j-no,k].Width:=TJSONObject(lItem.Items[l]).Get('ComponentDataSource', TJSONObject.Create(FTable[i,j].ComponentDataSource));
          //FTable[j-no,k].Width:=TJSONObject(lItem.Items[l]).Get('ComponentDataFieldName',TJSONObjectClass.Create(FTable[i,j].ComponentDataFieldName.ClassName));
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
    Frun:=0;
    DrawTable(FBuffer,FColWidth,FRowHeight,0,0);
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
  // 现在，我们创建一个新的JSON对象来写入数据
  jsonRoot := TJSONObject.Create;
  jsonRoot.Add('TQFGridPanelComponent', utf8toansi('TQFGridPanelComponent配置'));
  jsonRoot.Add('Version', Version);

  jsonGrid := TJSONObject.Create;
  jsonRoot.Add(self.Name, jsonGrid);//grid

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

  for i := 0 to FRowCount do
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
      jsonParamObj.Add('Width', FTable[i,j].Width);
      jsonParamObj.Add('Height', FTable[i,j].Height);
      jsonParamObj.Add('ColSpan', FTable[i,j].ColSpan);
      jsonParamObj.Add('RowSpan', FTable[i,j].RowSpan);
      jsonParamObj.Add('DispType', FTable[i,j].DispType);
      jsonParamObj.Add('str', FTable[i,j].str);
      jsonParamObj.Add('Color', FTable[i,j].Color);
      jsonParamObj.Add('Align', FTable[i,j].Align);
      jsonParamObj.Add('FontName', FTable[i,j].FontName);
      jsonParamObj.Add('FontSize', FTable[i,j].FontSize);
      jsonParamObj.Add('FontStyle', FTable[i,j].FontStyle);
      jsonParamObj.Add('FontColor', FTable[i,j].FontColor);
      jsonParamObj.Add('ComponentType', FTable[i,j].ComponentType);
      jsonParamObj.Add('FontSComponentNametyle', FTable[i,j].ComponentName);
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
  savejsonfile.SaveToFile(files);
  savejsonfile.Free;

  jsonGrid:=nil;
  jsonRow:=nil;
  jsonRoot:=nil;
end;


initialization
//
end.
