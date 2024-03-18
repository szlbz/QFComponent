{
秋风(QQ:315795176)开发的控件包，有2个控件：
TQFRichView：类RichView控件
TQFScrollingText：滚动显示控件
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
}
unit QFComponent;

interface

uses
  Classes, SysUtils, Forms, Controls,  Graphics, ExtCtrls,
  ComponentEditors,Dialogs, Printers,
  lclintf, LazFileUtils, lazutf8, LMessages,StrUtils,QFRichEdit;

type

  TLineType = record
     str:string;
     FontColor:TColor;
     FontStyle:integer;
     FontSize:integer;
     LineHeight:integer;
     Align:integer;
     DispType:string;
  end;

  TCellType = record
     str:string[255];
     FontName:string[20];
     FontStyle:byte;
     Color:TColor;
     Align:byte;
     Width:integer;
     Height:integer;
  end;

  TTableSL = record
     hs1:integer;
     hs2:integer;
     row:integer;
     col:integer;
  end;

  TCustomText = class(TCustomControl)
  private
    FTextHeigth:integer;
    //FQFRE:TQFRichEditor;
    isLeftButtonDown: Boolean;
    TTHNO:integer;
    FTS:integer;//表格数量
    FTablesl:array of TTableSL;
    FTable:Array of Array of TCellType;
    FOldFontSize:integer;
    FLineList:array of TLineType;
    FActive: boolean;
    FActiveLine: integer;
    FActiveLineHeight1:integer;
    FActiveLineHeightSave1:integer;
    FActiveLineHeight2:integer;
    FActiveLineHeightSave2:integer;
    FActiveLineSave:integer;
    FBuffer: TBitmap;
    IMG: TImage;
    FLineHeight: integer;
    FLines: TStrings;
    FOffset: integer;
    FStepSize: integer;
    Lineno:integer;
    FGapX:integer;
    FGapY:integer;
    FColor:TColor;
    function ActiveLineIsURL: boolean;
    procedure Init;
    procedure DrawScrollingText(Sender: TObject);
    procedure SetLines(const AValue: TStrings);
    procedure SetColor(const AValue: TColor);
    function GetStringTextWidth(Buffer: TBitmap;str:string):integer;
    procedure DisplayText(Buffer: TBitmap;x,y:integer;str:string);
    procedure DrawTexts(Buffer: TBitmap;y:integer);
    function DrawTable(Buffer: TBitmap;Index, y:integer):integer;
    function Deleteidentification(i:integer;str:string;out j:integer):string;//删除标识
    function TruncationStr(Buffer: TBitmap; str:string;fbwidth:integer):string;
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
    property Lines: TStrings read FLines write SetLines;
    property GapX: integer read FGapX write FGapX;
    property GapY: integer read FGapY write FGapY;
    property Color: TColor read FColor write SetColor;
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
    initialY: Integer; // 用于存储鼠标按下时的初始位置
    procedure SetLines(const AValue: TStrings);
    procedure DrawScrollingText(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SavePicture(Files:string);
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
begin
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
    CellType.Color := clBlue;
  end;
end;

function ReplaceCharacters(str:string):string; //删除所有特殊符号
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
  Result:=Result.Replace('<SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('<SUB>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUP>','',[rfReplaceAll, rfIgnoreCase]);
  Result:=Result.Replace('</SUB>','',[rfReplaceAll, rfIgnoreCase]);
end;

function TCustomText.GetStringTextWidth(Buffer: TBitmap;str:string):integer;
var
  x,i:integer;
  s1:string;
  oldFontSize,NewFontSize:integer;
  oldStyles:TFontStyles;
begin
  x:=0;
  if (pos('<C1>',str.ToUpper)>0) or
  (pos('<C2>',str.ToUpper)>0) or
  (pos('<C3>',str.ToUpper)>0) or
  (pos('<C4>',str.ToUpper)>0) or
  (pos('<C5>',str.ToUpper)>0) or
  (pos('<SUP>',str.ToUpper)>0) or
  (pos('<SUB>',str.ToUpper)>0) or
  (pos('</SUP>',str.ToUpper)>0) or
  (pos('</SUB>',str.ToUpper)>0) or
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


procedure TCustomText.Init;
var
  i,w,j,k,dc,rj:integer;
  str:string;
  s,s1,textstyle:string;
  Linetemp: TStringList;

  procedure preprocessing;
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
        FLineHeight:=FLineHeight+IMG.Picture.Height;
      end;
    end;
    //超链接
    if (Pos('http://', s) = 1) or (Pos('https://', s) = 1) then
    begin
      if i = FActiveLine then
      begin
        FLineList[i].FontStyle := 4;//fsUnderline;
        FLineList[i].FontColor := clRed;
      end
      else
        FLineList[i].FontColor := clBlue;
      FActiveLineHeight1:=FLineHeight;//超链接出现时的高度
      FActiveLineHeight2:=FLineHeight+FBuffer.Canvas.TextHeight(s); //超链接出现时的高度
      FActiveLineSave:=i;//超链接出现时的行数
    end;
  end;

  procedure TablePreprocessing;
  var
    i,j,row,col,js,gw:integer;
    no,k:integer;
  begin
    //解析是否包含表格
    //解析有几个表格
    FTS:=0;
    for i := 0 to FLines.Count-1 do
    begin
      s := Trim(FLines[i]);
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
    setlength(FTablesl,FTS);//FTS--表格数量
    ////////////////////////////////
    col:=0;
    row:=0;
    js:=0;
    no:=0;
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
  FLineHeight:=0;
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

  FActiveLineHeight1:=0;
  FActiveLineHeight2:=0;
  FActiveLineSave:=0;
  FLineHeight:=0;//2024.3.10
  FLineList:=nil;
  setlength(FLineList,Linetemp.Count);
  Lineno:= Linetemp.Count;
  for i := 0 to Linetemp.Count-1 do
  begin
    s := Linetemp[i];
    if Length(s) > 0 then
    begin
      preprocessing;
      FLineList[i].str:=s;
      //FBuffer.Canvas.Font.Style:=FLineList[i].FontStyle;
      FBuffer.Canvas.Font.Size:=FLineList[i].FontSize;
      FLineList[i].LineHeight:=FBuffer.Canvas.TextHeight(s);
      FLineHeight:=FLineHeight+FLineList[i].LineHeight;
    end;
  end;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  if FOffset = -1 then
  begin
    FOffset := FBuffer.Height;
    FActiveLineHeightSave1:=FActiveLineHeight1;
    FActiveLineHeightSave2:=FActiveLineHeight2; //2024-03-10
  end;

  with FBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;
  freeandnil(Linetemp);
end;

procedure TCustomText.DrawScrollingText(Sender: TObject);
begin
  with FBuffer.Canvas do
  begin
    Brush.Color := Fcolor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;
  FOffset:=0;
  DrawTexts(FBuffer,FOffset);
  Canvas.Draw(0,0,FBuffer)
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

//删除标识
function TCustomText.Deleteidentification(i:integer;str:string;out j:integer):string;
begin
  Result:=utf8copy(str,i,1);
  j:=0;
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
  s1:string;
  zwh,ywh:integer;
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
  (pos('<SUP>',str.ToUpper)>0) or
  (pos('<SUB>',str.ToUpper)>0) or
  (pos('</SUP>',str.ToUpper)>0) or
  (pos('</SUB>',str.ToUpper)>0) or
  (pos('</C>',str.ToUpper)>0) or
  (pos('<!>',str)>0) or
  (pos('<$>',str)>0) or
  (pos('<@>',str)>0) or
  (pos('<#>',str)>0) or
  (pos('</>',str)>0)
  then
  begin
    i:=0;
    oldColor:=Buffer.Canvas.font.Color;
    oldStyles:=Buffer.Canvas.font.Style;
    oldFontSize:=Buffer.Canvas.font.Size;
    oldy:=y;
    supy:=y;
    suby:=y+(Buffer.Canvas.TextHeight('国') div 2)-5;
    NewFontSize:=Buffer.Canvas.font.Size div 2;
    if NewFontSize=0 then NewFontSize:=5;
    while i<=utf8length(str) do
    begin
      s1:=utf8copy(str,i,1);
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
  x0,y0,x1,y1:integer;
begin
  row:=FTablesl[Index].row;
  col:=FTablesl[Index].col-1;
  Buffer.Canvas.Font.Style:=[fsBold];
  h:=Buffer.Canvas.TextHeight('国')+2;
  w:=(Buffer.Width-FGapX*2) div col;
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
    y0:=FOffset + y+FGapY+i*h;
    for j:=0 to col-1 do
    begin
      //设置字体属性
      if FTable[i,j+1].FontStyle=0 then Buffer.Canvas.Font.Style:=[];
      if FTable[i,j+1].FontStyle=1 then Buffer.Canvas.Font.Style:=[fsBold];
      if FTable[i,j+1].FontStyle=2 then Buffer.Canvas.Font.Style:=[fsStrikeOut];
      if FTable[i,j+1].FontStyle=3 then Buffer.Canvas.Font.Style:=[fsItalic];
      if FTable[i,j+1].FontStyle=4 then Buffer.Canvas.Font.Style:=[fsUnderline];
      Buffer.Canvas.Font.Color:=FTable[i,j+1].Color;
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
        y0:=FOffset + y+FGapY+i*h;
        Buffer.Canvas.Font.Style:=[fsBold];
        Buffer.Canvas.Font.Color:=FTable[i,j+1].Color;
        DisplayText(Buffer,x1+2, y0+5,TruncationStr(Buffer,FTable[i,j+1].str,w));//截断超过单元格宽度的字符串
      end
      else
      if i>1 then //跳过第2行--第2行定义单元格的对齐格式
      begin
         y0:=FOffset + y+FGapY+(i-1)*h;
         DisplayText(Buffer,x1+2, y0+5,TruncationStr(Buffer,FTable[i,j+1].str,w));
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
  i: integer;
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
          if (i<FActiveLineSave) and (TTHNO=-1) then
          begin
            TTHNO:=0;
            FActiveLineHeight1:=FActiveLineHeight1+Buffer.Canvas.TextHeight(FLineList[i].str)*(FTS-1); //超链接出现时的高度
            FActiveLineHeight2:=FActiveLineHeight1+Buffer.Canvas.TextHeight(FLineList[i].str); //超链接出现时的高度
          end;
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
        DisplayText(Buffer,FGapX, FOffset + y+FGapY, FLineList[i].str);
      if FLineList[i].Align=2 then //行居中
        DisplayText(Buffer,FGapX+(Buffer.Width - w) div 2, FOffset + y+FGapY, FLineList[i].str);
      if FLineList[i].Align=3 then //行居右
        DisplayText(Buffer,(Buffer.Width - w)-FGapX, FOffset + y+FGapY, FLineList[i].str);
      y:=y+ FLineList[i].LineHeight-FGapY*2+5;
    end;
  end;
  FTextHeigth:=y;
end;

function TCustomText.ActiveLineIsURL: boolean;
begin
  if (FActiveLine > 0) and (FActiveLine < Lineno) then
    Result := (Pos('http://', FLineList[FActiveLine].str) = 1) or (Pos('https://', FLineList[FActiveLine].str) = 1)
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
begin
  inherited MouseDown(Button, Shift, X, Y);

  if ActiveLineIsURL then
  begin
    OpenURL(FLineList[FActiveLine].str);
    isLeftButtonDown := False;
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
  FStepSize := 1;
  TTHNO:=-1;
  FOffset := -1;
  FGapX:=0;
  FGapY:=0;
  FOldFontSize:=FBuffer.Canvas.Font.Size;
  FColor:=clWhite;
end;

destructor TCustomText.Destroy;
begin
  if Assigned(FTable) then
    FTable:=nil;
  if Assigned(FLineList) then
  FLineList:=nil;
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
  with FBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;

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
  with FBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;

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
end;

destructor TQFScrollingText.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TQFScrollingText.SetActive(const AValue: boolean);
begin
  FActive := AValue;
  if FActive then
    Init;
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
    with FBuffer.Canvas do
    begin
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      FillRect(0, 0, Width, Height);
    end;
    FOffset:=0;
    DrawTexts(FBuffer,FOffset);
    Canvas.Draw(0,0,FBuffer)
  end;
end;

procedure TQFScrollingText.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if (Y>FBuffer.Height+FActiveLineHeight1) and (Y<FBuffer.Height+FActiveLineHeight2) then
      FActiveLine := FActiveLineSave
  else FActiveLine:= -1;

  Cursor := crDefault;

  if (FActiveLine >= 0) and (FActiveLine < Lineno) and ActiveLineIsURL then
    Cursor := crHandPoint;
end;

procedure TQFScrollingText.DoTimer(Sender: TObject);
begin
  if not Active then
    Exit;

  Dec(FOffset, FStepSize);
  Dec(FActiveLineHeight1, FStepSize);
  Dec(FActiveLineHeight2, FStepSize);
  with FBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;

  DrawTexts(FBuffer,0);
  if FOffset+FLineHeight=0 then
  begin
    FActiveLineHeight1:=FActiveLineHeightSave1;
    FActiveLineHeight2:=FActiveLineHeightSave2;
    FOffset := FBuffer.Height;
  end;

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
begin
  inherited WMMouseWheel(Message);

  if Message.WheelDelta<0 then //up
  begin
    if abs(FOffset)<FTextHeigth-FBuffer.Height+FStepSize+35 then
    begin
      Dec(FOffset, FStepSize);
      Dec(FActiveLineHeight1, FStepSize);
      Dec(FActiveLineHeight2, FStepSize);
    end;
  end
  else
  begin   //down
    if FOffset<0 then
    begin
      FOffset:=FOffset+FStepSize;
      FActiveLineHeight1:=FActiveLineHeight1+ FStepSize;
      FActiveLineHeight2:=FActiveLineHeight2+ FStepSize;
    end;
  end;
  with FBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;

  DrawTexts(FBuffer,0);
  Canvas.Draw(0,0,FBuffer);
end;

procedure TQFRichView.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Button = mbLeft then
  begin
    // 处理左键按下
    isLeftButtonDown := True;
    initialY := Y;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TQFRichView.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Button = mbLeft then
  begin
    // 处理左键释放
    isLeftButtonDown := False;
  end;
end;

procedure TQFRichView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  movedY: Integer;//存储鼠标移动的距离
begin
  inherited MouseMove(Shift, X, Y);

  if (Y>abs(FActiveLineHeight1)) and (Y<abs(FActiveLineHeight2)) then
    FActiveLine := FActiveLineSave
  else
    FActiveLine:= -1;

  Cursor := crDefault;

  if (FActiveLine >= 0) and (FActiveLine < Lineno) and ActiveLineIsURL then
    Cursor := crHandPoint;

  if isLeftButtonDown then
  begin
    movedY := Y - initialY; // 计算Y轴上的移动距离

    if movedY > 0 then
    begin
      // 鼠标向下移动
      if abs(FOffset)<(FTextHeigth-FBuffer.Height+35) then
      begin
        Dec(FOffset, abs(35));
        Dec(FActiveLineHeight1, abs(35));
        Dec(FActiveLineHeight2, abs(35));
      end;
    end
    else
    if movedY < 0 then
    begin
      // 鼠标向上移动
      if FOffset<0 then
      begin
        inc(FOffset, abs(35));
        inc(FActiveLineHeight1, abs(35));
        inc(FActiveLineHeight2, abs(35));
      end;
    end;

    with FBuffer.Canvas do
    begin
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      FillRect(0, 0, Width, Height);
    end;

    DrawTexts(FBuffer,0);
    Canvas.Draw(0,0,FBuffer);
  end;
end;

procedure TQFRichView.DrawScrollingText(Sender: TObject);
begin
  init;
  with FBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;
  FOffset:=0;
  DrawTexts(FBuffer,FOffset);
  Canvas.Draw(0,0,FBuffer)
end;

procedure TQFRichView.SavePicture(Files:string);
var im:TImage;
  FCanvas: TBitmap;
  ARect : TRect;
  oldFOffset:integer;
begin
  init;
  FCanvas:=TBitmap.Create;
  FCanvas.Height:=FTextHeigth;
  FCanvas.Width:=FBuffer.Width;
  ARect.Width:=Width;
  ARect.Height:=FTextHeigth;
  ARect.Left:=0;
  ARect.Top:=0;
  with FCanvas.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(ARect);  //保存图片时只能使用FillRect(ARect)才能有设定的背景色;
                      //不能用 FillRect(0, 0, Width, Height)，否则背景色是黑色的
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

initialization
//
end.
