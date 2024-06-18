unit PublicUnit;

interface

uses
  Classes, SysUtils,Controls,FPCanvas,  Graphics,db;

type

  TDispTypes = (dtText, dtPict, dtBookmak1, dtBookmak2, dtLink, dtComponent);

  TCellFontStyle  =(cfsNone,cfsBold,cfsStrikeOut,cfsItalic,cfsUnderline);

  CellAlign = (calNone, calLeft, calClient, calRight);

  TCell = record
     x:integer;
     y:integer;
     Gap:integer;
     Width:integer;
     Height:integer;
     ColMerge:integer; //从当前单元格向右合并n个单元格
     RowMerge:integer; //从当单元格向下合并n个单元格
     DispType:TDispTypes;//0--文字 1--图像 2-bookmark1 3-bookmark2 4--link 5--控件
     str:string[255];
     URL:string[200];
     bookmarkstr:string[7];
     Color:TColor;
     Align:CellAlign;
     FontName:string[20];
     FontSize:integer;
     FontColor:TColor;
     FontStyle:TCellFontStyle;
     ComponentType:string;
     ComponentName:string;
     ComponentDataSource:TDataSource;
     ComponentDataFieldName:TField;
     DrawTop : Boolean;       // 画顶线
     DrawLeft : Boolean;      // 画左线
     DrawBottom : Boolean;    // 画底线
     DrawRight : Boolean;     // 画右线
     TopLineStyle:TFPPenStyle;
     LeftLineStyle:TFPPenStyle;
     BottomLineStyle:TFPPenStyle;
     RightLineStyle:TFPPenStyle;
     LineStyle:TFPPenStyle;
     Visible:Boolean;//
  end;
//var
  //QFCellProperReturn:Boolean;

implementation

end.
