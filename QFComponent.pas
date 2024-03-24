{*******************************************************}
{                                                       }
{         支持Lazarus及windows、linux等平台             }
{          x86_64/aarch64/lonngarch64等CPU              }
{                                                       }
{                    类富文本控件                       }
{                 QFRichView 0.5                        }
{                                                       }
{    Copyright(c) 2024-2024 秋风(QQ315795176) 出品      }
{                                                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}
{控件包有2个控件：
采用自定义的类富文本，集编辑、显示和导出图片等功能。
TQFRichView：自定义的类富文本文字显示，类RichView控件；
TQFScrollingText：滚动内容采用自定义的类富文本文字显示的滚动显示控件；
说明：
1、控件的文字显示渲染等核心功能是秋风原创独立编写的。
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
  //ComponentEditors,
  lclintf, LazFileUtils, lazutf8, LMessages,StrUtils,QFRichEdit;

type

  TLineType = record
     str:string;
     FontColor:TColor;
     FontStyle:integer;
     FontSize:integer;
     LineHeight:integer;
     Align:integer;
     URL:string;
     DispType:string[10];
     BookMark1:string;
     BookMark2:string;
  end;

  TCellType = record
     str:string[255];
     FontName:string[20];
     bookmarkstr:string[7];
     FontStyle:byte;
     Color:TColor;
     Align:byte;
     Width:integer;
     Height:integer;
     ColSpan:integer;
     RowSpan:integer;
     DispType:integer;//0--文字 1--图像 2-bookmark1 3-bookmark2
  end;

  TTableSL = record
     hs1:integer;
     hs2:integer;
     row:integer;
     col:integer;
  end;

  THyperLink = record
     URL:string;
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

  TCustomText = class(TCustomControl)//TScrollingWinControl)//TCustomControl)
  private
    FRect:TRect;
    FMV:integer;
    FBMSL1:integer;//书签数量
    FBMSL2:integer;
    FLineSpacing:integer;//行距
    FTextHeigth:integer;
    //FQFRE:TQFRichEditor;
    FisLeftButtonDown: Boolean; //鼠标左键按下标识
    TTHNO:integer;
    FTS:integer;//表格数量
    FBackgroundImage:TImage;
    FBackImageFile:string;
    FBookMark1:array of TQFBookMark;
    FBookMark2:array of TQFBookMark;
    FHyperLink:array of THyperLink;
    FTablesl:array of TTableSL;
    FTable:Array of Array of TCellType;
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
    function ActiveLineIsURL: boolean;
    procedure Init;
    procedure BackgroundRefresh(Buffer:TBitmap);
    procedure DrawScrollingText(Sender: TObject);
    procedure SetLines(const AValue: TStrings);
    procedure SetColor(const AValue: TColor);
    function FindMark1(str:string;out NewStr:string):Boolean;
    function FindMark2(str:string;out NewStr:string):Boolean;
    function GetStringTextWidth(Buffer: TBitmap;str:string):integer;
    procedure DisplayText(Buffer: TBitmap;x,y:integer;str:string);
    procedure DrawTexts(Buffer: TBitmap;y:integer);
    function DrawTable(Buffer: TBitmap;Index, y:integer):integer;
    function Deleteidentification(i:integer;str:string;out j:integer):string;//删除标识
    function TruncationStr(Buffer: TBitmap; str:string;fbwidth:integer):string;
    function ReplaceCharacters(str:string):string; //删除所有特殊符号
    procedure GetTableInfo(no:integer);
    procedure GetFontStyle(s:string;out CellType:TCellType);
  protected
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
    property BackImageFile: string read FBackImageFile write FBackImageFile;
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

  TQFRichView =  class(TCustomText)
  private
    FinitialY: Integer; // 用于存储鼠标按下时的初始位置
    procedure SetLines(const AValue: TStrings);
    procedure DrawScrollingText(Sender: TObject);
  protected
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SavePicture(Files:string);
    procedure PrintPicture;
  published
    property StepSize: integer read FStepSize write FStepSize;
  end;

procedure Register;

implementation

{$R *.res}

procedure Register;
begin
  //RegisterClasses([TQFRichEditor]);
  //RegisterComponentEditor(TString,TQFRichEditor);
  RegisterComponents('QF Component', [TQFScrollingText,TQFRichView]);
end;

{ TScrollingText }

procedure TCustomText.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    FLines.Assign(AValue);
    Init;
  end;
end;

procedure TCustomText.SetColor(const AValue: TColor);
begin
  FColor:=AValue;
  Init;
  DrawTexts(FBuffer,0);
  Canvas.Draw(0,0,FBuffer)
end;

procedure TCustomText.GetFontStyle(s:string;out CellType:TCellType);
var s1:string;
  hlkstr:string;
  hlk:integer;
begin
  CellType.bookmarkstr:='';
  CellType.DispType:=0;
  CellType.FontStyle:=0;
  CellType.Color:=clBlack;
  CellType.str:=s;
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
  if pos('<BM',s.ToUpper)>0 then
  begin
    s1:=copy(s,pos('<BM',s.ToUpper),(pos('>',s.ToUpper)-pos('<BM',s.ToUpper)));
    s1:=copy(s,pos(s1,s)+1+length(s1),length(s));
    CellType.str:=s1;
    CellType.DispType:=2;
    s1:=copy(s,pos('<BM',s.ToUpper)+1,(pos('>',s.ToUpper)-pos('<BM',s.ToUpper)-1));
    CellType.bookmarkstr:=s1;
  end;
  if pos('[BM',s.ToUpper)>0 then
  begin
    s1:=copy(s,pos('[BM',s.ToUpper),(pos(']',s.ToUpper)-pos('[BM',s.ToUpper)));
    s1:=copy(s,pos(s1,s)+1+length(s1),length(s));
    CellType.str:=s1;
    CellType.DispType:=3;
    s1:=copy(s,pos('[BM',s.ToUpper)+1,(pos(']',s.ToUpper)-pos('[BM',s.ToUpper)-1));
    CellType.bookmarkstr:=s1;
  end;
  if (pos('<HLK>',s.ToUpper)>0) and (pos('</HLK>',s.ToUpper)>0) then
  begin
    hlkstr:=s;
    hlk:=pos('<HLK>',s.ToUpper)+5;
    hlkstr:=copy(s,hlk,pos('</HLK>',s.ToUpper)-hlk);
    CellType.str:=hlkstr;
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
var i:integer;
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
  Result:=Result.Replace('<SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<SUB>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUB>','',[rfReplaceAll, rfIgnoreCase]);
  if Assigned(FBookMark1) then
  begin
    for i:=0 to High(FBookMark1) do
    begin
      Result:=Result.Replace('<BM'+(i+1).ToString+'>','',[rfReplaceAll, rfIgnoreCase]);
      //Break;
    end;
  end;
  if Assigned(FBookMark2) then
  begin
    for i:=0 to High(FBookMark2) do
    begin
      Result:=Result.Replace('[BM'+(i+1).ToString+']','',[rfReplaceAll, rfIgnoreCase]);
      //Break;
    end;
  end;
end;

function TCustomText.FindMark1(str:string;out NewStr:string):Boolean;
var i:integer;
begin
  Result:=false;
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

function TCustomText.FindMark2(str:string;out NewStr:string):Boolean;
var i:integer;
begin
  Result:=false;
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

function TCustomText.GetStringTextWidth(Buffer: TBitmap;str:string):integer;
var
  x,i,j:integer;
  s1,newstr:string;
  oldFontSize,NewFontSize:integer;
  oldStyles:TFontStyles;
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
  (pos('</C>',str.ToUpper)>0) or
  (pos('<$>',str)>0) or
  (pos('<!>',str)>0) or
  (pos('<@>',str)>0) or
  (pos('<#>',str)>0) or
  (FindMark1(str,newstr)) or
  (FindMark2(str,newstr)) or
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
      if Assigned(FBookMark1) then
      begin
        for j:=0 to High(FBookMark1) do
        begin
          if pos('<BM'+(j+1).ToString+'>',s1.ToUpper)>0 then
          begin
            i:=i+length('<BM'+(j+1).ToString+'>');
            Break;
          end;
        end;
      end;
      if Assigned(FBookMark2) then
      begin
        for j:=0 to High(FBookMark2) do
        begin
          if pos('<BM'+(j+1).ToString+'>',s1.ToUpper)>0 then
          begin
            i:=i+length('[BM'+(j+1).ToString+']');
            Break;
          end;
        end;
      end;

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
  if trim(FBackImageFile)<>'' then
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

procedure TCustomText.Init;
var
  i,w,j,k,dc,rj,hls:integer;
  kmsl1,kmsl2:integer;
  str:string;
  s,s1,textstyle:string;
  Linetemp: TStringList;
  hlk:integer;
  procedure preprocessing;
  var newbmstr,nbms:string;
    hlkstr:string;
    hlk:integer;
  begin
    FLineList[i].DispType:='';
    FLineList[i].FontSize:=FOldFontSize;
    FLineList[i].Align :=1;
    FLineList[i].FontStyle:=0;
    //行对齐模式
    if pos('[L]',s.ToUpper)>0 then
    begin
     textstyle:=textstyle+'[L]';
     s:=s.Replace('[L]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
     FLineList[i].Align :=1;
    end;
    if pos('[C]',s.ToUpper)>0 then
    begin
     textstyle:=textstyle+'[C]';
     s:=s.Replace('[C]','',[rfReplaceAll,rfIgnoreCase]);
     FLineList[i].Align :=2;
    end;
    if pos('[R]',s.ToUpper)>0 then
    begin
     textstyle:=textstyle+'[R]';
     s:=s.Replace('[R]','',[rfReplaceAll,rfIgnoreCase]);
     FLineList[i].Align :=3;
    end;
    //字体风格
    if pos('[#]',s.ToUpper)>0 then
    begin
      textstyle:=textstyle+'[#]';
      s:=s.Replace('[#]','',[rfReplaceAll,rfIgnoreCase]);
      FLineList[i].FontStyle :=1;// fsBold;
    end;
    if pos('[@]',s)>0 then
    begin
      textstyle:=textstyle+'[@]';
      s:=s.Replace('[@]','',[rfReplaceAll,rfIgnoreCase]);
      FLineList[i].FontStyle :=2;// fsStrikeOut;
    end;
    if pos('[$]',s)>0 then
    begin
      textstyle:=textstyle+'[$]';
      s:=s.Replace('[$]','',[rfReplaceAll,rfIgnoreCase]);
      FLineList[i].FontStyle :=3;// fsItalic;
    end;
    if pos('[!]',s)>0 then
    begin
      textstyle:=textstyle+'[!]';
      s:=s.Replace('[!]','',[rfReplaceAll,rfIgnoreCase]);
      FLineList[i].FontStyle :=4;// fsUnderline;
    end;
    //字体颜色
    if pos('[C1]',s.ToUpper)>0 then
    begin
      textstyle:=textstyle+'[C1]';
       s:=s.Replace('[C1]','',[rfReplaceAll,rfIgnoreCase]);
       FLineList[i].FontColor := clBlack;
    end;
    if pos('[C2]',s.ToUpper)>0 then
    begin
      textstyle:=textstyle+'[C2]';
       s:=s.Replace('[C2]','',[rfReplaceAll,rfIgnoreCase]);
       FLineList[i].FontColor := clRed;
    end;
    if pos('[C3]',s.ToUpper)>0 then
    begin
      textstyle:=textstyle+'[C3]';
       s:=s.Replace('[C3]','',[rfReplaceAll,rfIgnoreCase]);
       FLineList[i].FontColor := clYellow;
    end;
    if pos('[C4]',s.ToUpper)>0 then
    begin
      textstyle:=textstyle+'[C4]';
       s:=s.Replace('[C4]','',[rfReplaceAll,rfIgnoreCase]);
       FLineList[i].FontColor := clGreen;
    end;
    if pos('[C5]',s.ToUpper)>0 then
    begin
       textstyle:=textstyle+'[C5]';
       s:=s.Replace('[C5]','',[rfReplaceAll,rfIgnoreCase]);
       FLineList[i].FontColor := clBlue;
    end;
    //字体尺寸
    if pos('[S1]',s.ToUpper)>0 then
    begin
      s:=s.Replace('[S1]','',[rfReplaceAll,rfIgnoreCase]);
      textstyle:=textstyle+'[S1]';
      FLineList[i].FontSize := 9
    end;
    if pos('[S2]',s.ToUpper)>0 then
    begin
      s:=s.Replace('[S2]','',[rfReplaceAll,rfIgnoreCase]);
      textstyle:=textstyle+'[S2]';
      FLineList[i].FontSize := 12
    end;
    if pos('[S3]',s.ToUpper)>0 then
    begin
      s:=s.Replace('[S3]','',[rfReplaceAll,rfIgnoreCase]);
      textstyle:=textstyle+'[S3]';
      FLineList[i].FontSize := 14
    end;
    if pos('[S4]',s.ToUpper)>0 then
    begin
      s:=s.Replace('[S4]','',[rfReplaceAll,rfIgnoreCase]);
      textstyle:=textstyle+'[S4]';
      FLineList[i].FontSize := 16
    end;
    if pos('[S5]',s.ToUpper)>0 then
    begin
      s:=s.Replace('[S5]','',[rfReplaceAll,rfIgnoreCase]);
      textstyle:=textstyle+'[S5]';
      FLineList[i].FontSize := 18;
    end;
    //分割线
    if (pos('[LINE]',s.ToUpper)>0) or (pos('***',s.ToUpper)>0) or
       (pos('___',s.ToUpper)>0) or (pos('---',s.ToUpper)>0) then
    begin
      textstyle:=textstyle+'[LINE]';
      FLineList[i].DispType:='LINE';
      s:=s.Replace('[LING]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    end;
    //双分割线
    if pos('[2LINE]',s.ToUpper)>0 then
    begin
      textstyle:=textstyle+'[2LINE]';
      FLineList[i].DispType:='2LINE';
      s:=s.Replace('[2LING]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
    end;
    //图像
    if pos('[IMG]',s.ToUpper)>0 then
    begin
      textstyle:=textstyle+'[IMG]';
      FLineList[i].DispType:='IMG';
      s:=s.Replace('[IMG]','',[rfReplaceAll,rfIgnoreCase]);//全部替换，忽略大小写
      if  FileExists(s) then
      begin
        IMG.Picture.LoadFromFile(s);
      end;
    end;
    //超链接
    if (Pos('HTTP://', s.ToUpper) >= 1) or (Pos('HTTPS://', s.ToUpper) >= 1) then
    begin
      hlkstr:=s;
      if pos('<HLK>',s.ToUpper)>0 then
      begin
        hlk:=pos('<HLK>',s.ToUpper)+5;
        hlkstr:=copy(s,hlk,pos('</HLK>',s.ToUpper)-hlk);
      end;
      FLineList[i].url:=hlkstr;
      FLineList[i].DispType:='URL';
      if i = FActiveLine then
      begin
        FLineList[i].FontStyle := 4;//fsUnderline;
        FLineList[i].FontColor := clRed;
      end
      else
        FLineList[i].FontColor := clBlue;
    end
    else
    if (Pos('<BM', s.ToUpper)>0) then
    begin
      FindMark1(s,newbmstr);
      nbms:=copy(s,pos('<BM',s.ToUpper)+1,(pos('>',s.ToUpper)-pos('<BM',s.ToUpper)-1));
      textstyle:=textstyle+'<'+nbms+'>';
      s:=newbmstr;
      FLineList[i].BookMark1:=nbms;
      FLineList[i].DispType:='BOOKMARK1';
      FLineList[i].FontColor := clBlue;
    end;
    if (Pos('[BM', s.ToUpper)>0) then
    begin
      FindMark2(s,newbmstr);
      nbms:=copy(s,pos('[BM',s.ToUpper)+1,(pos(']',s.ToUpper)-pos('[BM',s.ToUpper)-1));
      textstyle:=textstyle+'['+nbms+']';
      s:=newbmstr;
      FLineList[i].BookMark2:=nbms;
      FLineList[i].DispType:='BOOKMARK2';
    end;
  end;

  procedure TablePreprocessing;
  var
    i,j,row,col,js,gw:integer;
    no,k,hl:integer;
  begin
    //解析是否包含表格
    //解析有几个表格
    FTS:=0;
    hl:=0;
    FBMSL1:=0; //书签数量
    FBMSL2:=0; //书签数量
    for i := 0 to FLines.Count-1 do
    begin
      s := Trim(FLines[i]);
      if (pos('[BM',s.ToUpper)>0) and (pos(']',s.ToUpper)>0) then
      begin
        inc(FBMSL2);//书签数量
      end
      else
      if (pos('<BM',s.ToUpper)>0) and (pos('>',s.ToUpper)>0) then
      begin
        inc(FBMSL1);//书签数量
      end
      else
      if (pos('HTTP',s.ToUpper)>0) then //URL数量
      begin
         inc(hl);
      end
      else
      if pos('|',s)>0 then
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
      s := Trim(FLines[i]);
      if pos('|',s)>0 then
      begin
        for j:=0 to utf8length(s) do
        begin
          if utf8copy(s,j,1)='|' then  //表格
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
          inc(no);
          row:=0;
          col:=0;
          js:=0;
        end;
      end;
      js:=0;
    end;
  end;
begin
  Lineno:=0;
  Linetemp:=TStringList.Create;
  setlength(FLineList,FLines.Count);
  k:= FLines.Count;
  TablePreprocessing;//表格预处理

  //根据控件宽度进行自动换行处理
  for i := 0 to FLines.Count-1 do
  begin
    s := FLines[i];
    textstyle:='';
    rj:=0;
    if Length(s) > 0 then
    begin
      preprocessing;
      FBuffer.Canvas.Font.Size:=FLineList[i].FontSize;
      w:=FBuffer.Canvas.TextWidth(ReplaceCharacters(s));
      if w>Width then //换行
      begin
         s1:='';
         k:=0;
         j:=1;
         while j<= utf8length(s) do
         begin
           s1:=s1+utf8copy(s,j,1);
           if FBuffer.Canvas.TextWidth(ReplaceCharacters(s1)+Deleteidentification(j,s,rj))+FGapX*2>=Width then //跳过特定符号
           begin
             if Deleteidentification(j,s,rj)=''then
               s1:=s1+utf8copy(s,j+1,rj);//3);
             Linetemp.Add(textstyle+s1);
             s1:='';
           end;
           if Deleteidentification(j,s,rj)='' then
           begin
             s1:=s1+utf8copy(s,j+1,rj);//3);
             j:=j+rj+1;//4;
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
  kmsl1:=0;
  kmsl2:=0;
  for i := 0 to Linetemp.Count-1 do
  begin
    s := Linetemp[i];
    if Length(s) > 0 then
    begin
      preprocessing;
      FLineList[i].str:=s;
      FBuffer.Canvas.Font.Size:=FLineList[i].FontSize;
      FLineList[i].LineHeight:=FBuffer.Canvas.TextHeight(s)+FLineSpacing;

      if FLineList[i].DispType='URL' then
      begin
        FHyperLink[hls].URL:= FLineList[i].url;
        FHyperLink[hls].hs:=i;//所在行数
        inc(hls);
      end
      else
      if FLineList[i].DispType='BOOKMARK1' then
      begin
         FBookMark1[kmsl1].BookMark:=FLineList[i].BookMark1;
         FBookMark1[kmsl1].hs:=i;//所在行数
         inc(kmsl1);
      end
      else
      if FLineList[i].DispType='BOOKMARK2' then
      begin
        FBookMark2[kmsl2].BookMark:=FLineList[i].BookMark2;
        FBookMark2[kmsl2].hs:=i;//所在行数
        inc(kmsl2);
      end;
    end;
  end;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  if FOffset = -1 then
    FOffset := FBuffer.Height;

  freeandnil(Linetemp);
  BackgroundRefresh(FBuffer); //刷新背景
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
  MyCellType:TCellType;
begin
  //解释表格有几行几列
{
|姓名|单位|地址|电话|
|:-:|:-|-:|:-:|
|秋风1|检测中心1|南山建工村1|183233640|
|秋风2|检测中心2|南山建工村2|283233640|
|秋风3|检测中心3|南山建工村3|383233640|
}
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
                GetfontStyle(str,MyCellType);
                FTable[row,col].str:= MyCellType.str;
                FTable[row,col].DispType:= MyCellType.DispType;
                FTable[row,col].bookmarkstr:= MyCellType.bookmarkstr;
                FTable[row,col].Align:=MyCellType.Align;
                FTable[row,col].Color:=MyCellType.Color;
                FTable[row,col].FontStyle:=MyCellType.FontStyle;
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

//删除标识(表格文字)
function TCustomText.Deleteidentification(i:integer;str:string;out j:integer):string;
var k:integer;
begin
  Result:=utf8copy(str,i,1);
  j:=0;
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

procedure TCustomText.DisplayText(Buffer: TBitmap;x,y:integer;str:string);
var i:integer;
  s1,newstr:string;
  zwh,ywh,k:integer;
  oldFontSize,NewFontSize:integer;
  oldColor:TColor;
  oldStyles:TFontStyles;
  oldy,supy,suby:integer;
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
  (FindMark1(str,newstr)) or
  (FindMark2(str,newstr)) or
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
    while i<=utf8length(str) do
    begin
      s1:=utf8copy(str,i,1);
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='B') and //书签目录
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
      if (s1='[') and (utf8copy(str,i+1,1).ToUpper='B') and//书签
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
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='S') and (utf8copy(str,i+2,1).ToUpper='U')
         and (utf8copy(str,i+3,1).ToUpper='P') and (utf8copy(str,i+4,1)='>') then
      begin
        Buffer.Canvas.font.Size:=NewFontSize;//上标
        y:=supy;
        i:=i+5;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='S') and (utf8copy(str,i+2,1).ToUpper='U')
         and (utf8copy(str,i+3,1).ToUpper='B') and (utf8copy(str,i+4,1)='>') then
      begin
        Buffer.Canvas.font.Size:=NewFontSize;//下标
        y:=suby;
        i:=i+5;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='/')  and (utf8copy(str,i+2,1).ToUpper='S')
         and (utf8copy(str,i+3,1).ToUpper='U')
         and (utf8copy(str,i+4,1).ToUpper='P') and (utf8copy(str,i+5,1)='>') then
      begin
        Buffer.Canvas.font.Size:=oldFontSize;//取消上标
        y:=oldy;
        i:=i+6;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1).ToUpper='/')  and (utf8copy(str,i+2,1).ToUpper='S')
         and (utf8copy(str,i+3,1).ToUpper='U')
         and (utf8copy(str,i+4,1).ToUpper='B') and (utf8copy(str,i+5,1)='>') then
      begin
        Buffer.Canvas.font.Size:=oldFontSize;//取消下标
        y:=oldy;
        i:=i+6;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='$') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsItalic];//斜体
        i:=i+3;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='!') and (utf8copy(str,i+2,1)='>') then
      begin
        Buffer.Canvas.font.Style:=[fsUnderline];//下划线
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
        if (utf8copy(str,i+2,1)='1') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clBlack;
        if (utf8copy(str,i+2,1)='2') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clRed;
        if (utf8copy(str,i+2,1)='3') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clYellow;
        if (utf8copy(str,i+2,1)='4') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clGreen;
        if (utf8copy(str,i+2,1)='5') and (utf8copy(str,i+3,1)='>') then Buffer.Canvas.font.Color:=clBlue;
        i:=i+4;
      end
      else
      if (s1='<') and (utf8copy(str,i+1,1)='/') and (utf8copy(str,i+2,1).ToUpper='C') and (utf8copy(str,i+3,1)='>') then
      begin
        Buffer.Canvas.font.Color:=oldColor;
        i:=i+4;
      end
      else
      begin
        ywh:=0;
        if s1<>'' then
        begin
          if ord(s1[1])<127 then
          begin
            ywh:=Buffer.Canvas.TextHeight(s1);
            zwh:=Buffer.Canvas.TextHeight('国');
            if ywh<>zwh then ywh:=abs(zwh-ywh) div 2
            else ywh:=0;
          end;
        end;
        Buffer.Canvas.TextOut(x, y+ywh, s1);//在linux中文和英文显示高度有差别
        x:=x+Buffer.Canvas.TextWidth(s1);
        inc(i);
      end;
    end;
  end
  else
    Buffer.Canvas.TextOut(x, y, str);
end;

//将超过单元格宽度的字符串截断
function TCustomText.TruncationStr(Buffer: TBitmap; str:string;fbwidth:integer):string;
var w1,rj,i:integer;
    tmp:string;
begin
   Result:=str;
   tmp:='';
   rj:=0;
   w1:=Buffer.Canvas.TextWidth(ReplaceCharacters(str))+5;
   if w1>fbwidth then
   begin
      tmp:='';
      i:=1;
      while i<= utf8length(str) do
      begin
        tmp:=tmp+utf8copy(str,i,1);
        if Buffer.Canvas.TextWidth(ReplaceCharacters(tmp)+Deleteidentification(i,str,rj))+5>fbwidth then //跳过特定符号
        begin
          Result:=tmp;
          Break;
        end;
        if Deleteidentification(i,str,rj)='' then
        begin
          tmp:=tmp+utf8copy(str,i+1,rj);
          i:=i+rj+1;
        end
        else
          inc(i);
      end;
   end;
end;

function TCustomText.DrawTable(Buffer: TBitmap;Index,y:integer):integer;
var i,j,w,h,row,col:integer;
    k:integer;
  x0,y0,x1,y1:integer;
  th:integer;//文字高度
  //:TRect;
begin
  row:=FTablesl[Index].row;
  col:=FTablesl[Index].col-1;
  Buffer.Canvas.Font.Style:=[fsBold];
  th:= Buffer.Canvas.TextHeight('国');
  h:=round(th*1.5); //表格行高
  w:=(Buffer.Width-FGapX*2) div col; //单元格宽
  Buffer.Canvas.Pen.Color:=clBlack;//黑色画笔
  for i:=0 to row-1 do  //画横线
  begin
    Buffer.Canvas.Line(FGapX,FOffset + y+FGapY+3+i*h,Buffer.Width-FGapX,FOffset + y+FGapY+3+i*h);
  end;
  for j:=0 to col do//画竖线
  begin
    if j<col then
      x0:=FGapX+j*w
    else
      x0:=Buffer.Width-FGapX;
    y0:=FOffset + y+FGapY+3;
    y1:=FOffset + y+FGapY+3+(row-1)*h;
    Buffer.Canvas.Line(
      x0,
      y0,
      x0,
      y1);
  end;
  for i:=0 to row-1 do  //绘表格文字
  begin
    for j:=0 to col-1 do
    begin
      if (FTable[i,j+1].DispType=0) or (FTable[i,j+1].DispType=2)
         or (FTable[i,j+1].DispType=3) or (FTable[i,j+1].DispType=4) then  //显示文字
      begin
        //设置字体属性
        if FTable[i,j+1].FontStyle=0 then Buffer.Canvas.Font.Style:=[];
        if FTable[i,j+1].FontStyle=1 then Buffer.Canvas.Font.Style:=[fsBold];
        if FTable[i,j+1].FontStyle=2 then Buffer.Canvas.Font.Style:=[fsStrikeOut];
        if FTable[i,j+1].FontStyle=3 then Buffer.Canvas.Font.Style:=[fsItalic];
        if FTable[i,j+1].FontStyle=4 then Buffer.Canvas.Font.Style:=[fsUnderline];
        if FTable[i,j+1].DispType>1 then  Buffer.Canvas.Font.Color:=clBlue //URL,bookmark
        else Buffer.Canvas.Font.Color:=FTable[i,j+1].Color;
        //设置字体属性

        x0:=FGapX+j*w;
        x1:=x0; //居左
        if FTable[0,j+1].Align=1 then
          x1:=x0 ;//居左
       if FTable[0,j+1].Align=2 then
          x1:=x0+(w-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j+1].str,w))) div 2; //居中
        if FTable[0,j+1].Align=3 then
           x1:=x0+(w-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j+1].str,w)))-5; //居右
        if i=0 then
        begin
          x1:=x0+(w-GetStringTextWidth(Buffer,TruncationStr(Buffer,FTable[i,j+1].str,w))) div 2;//标题行文字居中
          y0:=FOffset + y+FGapY+i*h+abs(h- th) div 2;//垂直居中
          Buffer.Canvas.Font.Style:=[fsBold];
          Buffer.Canvas.Font.Color:=FTable[i,j+1].Color;
          DisplayText(Buffer,x1+2, y0+5,TruncationStr(Buffer,FTable[i,j+1].str,w));//截断超过单元格宽度的字符串
        end
        else
        if i>1 then //跳过第2行--第2行定义单元格的对齐格式
        begin
           y0:=FOffset + y+FGapY+(i-1)*h+abs(h- th) div 2;//垂直居中
           DisplayText(Buffer,x1+2, y0+5,TruncationStr(Buffer,FTable[i,j+1].str,w));
        end;
      end;

      if FTable[i,j+1].DispType=4 then //URL
      begin
        for k:=0 to high(FHyperLink) do
        begin
          if FHyperLink[k].url=FTable[i,j+1].str then
          begin
            FHyperLink[k].x1:=x1;
            FHyperLink[k].x2:=x1+Buffer.Canvas.TextWidth(FTable[i,j+1].str);
            FHyperLink[k].y1:=y+FGapY+(i-1)*h+abs(h- th) div 2;
            FHyperLink[k].y2:=y+FGapY+(i-1)*h+(abs(h- th) div 2)+Buffer.Canvas.TextHeight(FLineList[i].str); //超链接出现时的高度;
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
            FBookMark1[k].x2:=x1+Buffer.Canvas.TextWidth(FTable[i,j+1].str);
            FBookMark1[k].y1:=y+FGapY+(i-1)*h+abs(h- th) div 2;
            FBookMark1[k].y2:=y+FGapY+(i-1)*h+(abs(h- th) div 2)+Buffer.Canvas.TextHeight(FLineList[i].str); //书签目录的高度;
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
            FBookMark2[k].y1:= y+FGapY+(i-1)*h+abs(h- th) div 2;
            FBookMark2[k].y2:= y+FGapY+(i-1)*h+(abs(h- th) div 2)+Buffer.Canvas.TextHeight(FLineList[i].str); //书签的高度;
            Break;
          end;
        end;
      end
      else
      if FTable[i,j+1].DispType=1 then  //显示图形
      begin
        x0:=FGapX+j*w+1;
        y0:=FOffset + y+FGapY+(i-1)*h+4;
        if  FileExists(FTable[i,j+1].str) then
        begin
          IMG.Picture.LoadFromFile(FTable[i,j+1].str);
          //设置图像显示位置及尺寸（单元格大小）
          FRect.Top:=y0;
          FRect.Left:=x0;
          FRect.Width:=w-1;
          FRect.Height:=h-1;
          Buffer.Canvas.StretchDraw(FRect,img.Picture.Bitmap);
        end
        else
        begin
          //没找到图像文件
          DisplayText(Buffer,x0+2, y0,TruncationStr(Buffer,'['+ExtractFileName(FTable[i,j+1].str+']'),w));
        end;
      end;
    end;
    FTable[i,0].Height:=Buffer.Canvas.TextHeight('国')+2;
  end;
  y:=y+(row-1)*h+5;
  Result:=y;
end;

procedure TCustomText.DrawTexts(Buffer: TBitmap;y:integer);
var
  w,x,disptable: integer;
  s: string;
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
      DisplayText(Buffer,x, FOffset + y+FGapY, FLineList[i].str);
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
      if FLineList[i].DispType='URL' then
      begin
        for k:=0 to high(FHyperLink) do
        begin
          if FHyperLink[k].hs=i then
          begin
            FHyperLink[k].x1:=x;
            FHyperLink[k].x2:=x+Buffer.Canvas.TextWidth(FLineList[i].str);
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
    Result := (Pos('http://', FLineList[FActiveLine].str) >= 1) or (Pos('https://', FLineList[FActiveLine].str) >= 1)
  else
    Result := False;
end;

procedure TCustomText.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;

  TTHNO:=-1;
  Init;
end;

procedure TCustomText.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var k,k1,oldy1,oldy2:integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if ActiveLineIsURL then
  begin
    OpenURL(FLineList[FActiveLine].URL);
    FisLeftButtonDown := False;
  end;
  if FBMActiveStr<>'' then //跳转到指定书签的位置
  begin
      if Assigned(FBookMark2) then
      begin
        for k:=0 to high(FBookMark2) do
        begin
          if FBookMark2[k].BookMark=FBMActiveStr then
          begin
            BackgroundRefresh(FBuffer);//刷新背景
            FOffset:=0;
            oldy1:=FBookMark2[k].y1;
            oldy2:=FBookMark2[k].y2;
            DrawTexts(FBuffer,-FBookMark2[k].y1);
            Canvas.Draw(0,0,FBuffer);
            FOffset:=-oldy1;
            for k1:=0 to high(FBookMark2) do
            begin
              FBookMark2[k1].y1:=-oldy1;
              FBookMark2[k1].y2:=-oldy2;
            end;
            break;
          end;
        end;
      end;
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

  OnPaint := @DrawScrollingText;
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
  FBMSL1:=0;
  FBMSL2:=0;
  FOldFontSize:=FBuffer.Canvas.Font.Size;
  FColor:=clWhite;
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

  init;
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
  init;
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
    Init;
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
    Init;
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
    init;
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
      if (y>abs(FOffset+FHyperLink[k].y1)) and (y<abs(FOffset+FHyperLink[k].y2)) then
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

constructor TQFRichView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnPaint := @DrawScrollingText;
  FStepSize := 10;
end;

destructor TQFRichView.Destroy;
begin
  inherited Destroy;
end;

procedure TQFRichView.SetLines(const AValue: TStrings);
begin
  if (AValue <> nil) then
  begin
    FLines.Assign(AValue);
    Init;
  end;
end;
procedure TQFRichView.WMMouseWheel(var Message: TLMMouseEvent);
var k:integer;
begin
  inherited WMMouseWheel(Message);

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
begin
  if Button = mbLeft then
  begin
    // 处理左键按下
    FisLeftButtonDown := True;
    FinitialY := Y;
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
  if Assigned(FHyperLink) then   //URL
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
  if Assigned(FBookMark1) then  //书签
  begin
    for k:=0 to high(FBookMark1) do
    begin
      if (y>abs(FOffset+FBookMark1[k].y1)) and (y<abs(FOffset+FBookMark1[k].y2)) and
         (x>FBookMark1[k].x1) and (x<FBookMark1[k].x2) then
      begin
        FBMActiveLine := FBookMark1[k].hs;
        FBMActiveStr := FBookMark1[k].BookMark;
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
  init;
  FOffset:=0;
  DrawTexts(FBuffer,FOffset);
  Canvas.Draw(0,0,FBuffer)
end;

procedure TQFRichView.SavePicture(Files:string);
var im:TImage;
  FCanvas: TBitmap;
  oldFOffset:integer;
begin
  init;
  FCanvas:=TBitmap.Create;
  FCanvas.Height:=FTextHeigth;
  FCanvas.Width:=FBuffer.Width;
  FRect.Width:=Width;
  FRect.Height:=FTextHeigth;
  FRect.Left:=0;
  FRect.Top:=0;
  if trim(FBackImageFile)<>'' then
  begin
    if FBackgroundImage<>nil then
      FCanvas.Canvas.StretchDraw(FRect,FBackgroundImage.Picture.Bitmap)
    else
    begin
      with FCanvas.Canvas do
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
    with FCanvas.Canvas do
    begin
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      FillRect(FRect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
                        //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
    end;
  end;
  oldFOffset:=FOffset;
  FOffset:=0;
  DrawTexts(FCanvas,0);

  im:=TImage.Create(nil);
  im.Picture.Jpeg.Assign(FCanvas);
  im.Picture.SaveToFile(Files);
  im.Free;
  FCanvas.Free;
  FOffset:=oldFOffset;
end;

procedure TQFRichView.PrintPicture;
var im:TImage;
  FCanvas: TPrinterCanvas;//TBitmap;
  oldFOffset:integer;
begin
  init;
  //FCanvas:=TPrinterCanvas.Create(nil);
  //FCanvas.p .PageHeight:=FTextHeigth;
  //FCanvas.PageWidth:=FBuffer.Width;
  //FRect.Width:=Width;
  //FRect.Height:=FTextHeigth;
  //FRect.Left:=0;
  //FRect.Top:=0;
  //if trim(FBackImageFile)<>'' then
  //begin
  //  if FBackgroundImage<>nil then
  //    FCanvas.Canvas.StretchDraw(FRect,FBackgroundImage.Picture.Bitmap)
  //  else
  //  begin
  //    with FCanvas.Canvas do
  //    begin
  //      Brush.Color := FColor;
  //      Brush.Style := bsSolid;
  //      FillRect(FRect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
  //                        //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
  //    end;
  //  end;
  //end
  //else
  //begin
  //  with FCanvas.Canvas do
  //  begin
  //    Brush.Color := FColor;
  //    Brush.Style := bsSolid;
  //    FillRect(FRect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
  //                      //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
  //  end;
  //end;
  //oldFOffset:=FOffset;
  //FOffset:=0;
  //DrawTexts(FCanvas,0);
  //
  //im:=TImage.Create(nil);
  //im.Picture.Jpeg.Assign(FCanvas);
  //im.Picture.SaveToFile(Files);
  //im.Free;
  //FCanvas.Free;
  //FOffset:=oldFOffset;
end;

initialization
//
end.
