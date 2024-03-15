unit SetTable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  MaskEdit, Buttons, StdCtrls, Grids, Tools;

type

  { TfrmTable }

  TfrmTable = class(TForm)
    cmdInsert: TButton;
    cmdClose: TButton;
    cboCellAlign: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    lblRows: TLabeledEdit;
    lblCells: TLabeledEdit;
    StringGrid1: TStringGrid;
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdInsertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblCellsChange(Sender: TObject);
    procedure lblRowsChange(Sender: TObject);
  private
    procedure MakeTableCode;
    procedure ShowErr(S: string);
    function IsNumberX(S: string): boolean;
  public

  end;

var
  frmTable: TfrmTable;

implementation

{$R *.lfm}

{ TfrmTable }

function TfrmTable.IsNumberX(S: string): boolean;
var
  iValue, iCode: integer;
begin

  iValue := 0;

  Val(S, iValue, iCode);

  if iCode = 0 then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TfrmTable.ShowErr(S: string);
begin
  MessageDlg('', S + ' 不是正确的整数.', mtError, [mbOK], '');
end;

procedure TfrmTable.MakeTableCode;
var
  R, C, X, Y: integer;
  sHeader: string;
  sAlign: string;
  sCells: string;
  h,sRow: string;
begin
  X := 1;
  Y := 1;

  sHeader := '|';
  sRow := '';
  sCells := '';

  R := StrToInt(lblRows.Text);
  C := StrToInt(lblCells.Text);

  if cboCellAlign.ItemIndex = 0 then
  begin
    sAlign := ':-';
  end;
  if cboCellAlign.ItemIndex = 1 then
  begin
    sAlign := '-:';
  end;
  if cboCellAlign.ItemIndex = 2 then
  begin
    sAlign := ':-:';
  end;

  //Do headers
  while Y <= C do
  begin
    h:=StringGrid1.Cells[y-1,0]+'|';
    if trim(h)='' then h:=' 标题 |' ;
    sHeader := sHeader + h;
    Inc(Y);
  end;
  sHeader := sHeader + sLineBreak + '|';

  //Do Alignment
  Y := 1;

  while Y <= C do
  begin
    sHeader := sHeader + sAlign + '|';
    Inc(Y);
  end;

  sHeader := sHeader + sLineBreak;
  //Build rows and cells
  while X <= R do
  begin
    for Y := 1 to C do
    begin
      h:=StringGrid1.Cells[y-1,x-1]+'|';
      if trim(h)='' then h:='foo   |' ;
      sCells := sCells + h;
    end;

    sRow := sRow + '|' + sCells + sLineBreak;
    sCells := '';
    Inc(X);
  end;

  //Store the table code.
  Tools.TableCode := sLineBreak+sHeader + sRow + sLineBreak;

  sCells := '';
  sAlign := '';
  sRow := '';
end;

procedure TfrmTable.FormCreate(Sender: TObject);
begin
  cboCellAlign.ItemIndex := 0;
end;

procedure TfrmTable.lblCellsChange(Sender: TObject);
var i,e:integer;
begin
  val(lblcells.Text,i,e);
  StringGrid1.ColCount:=i;
end;

procedure TfrmTable.lblRowsChange(Sender: TObject);
var i,e:integer;
begin
  val(lblRows.Text,i,e);
  StringGrid1.RowCount:=i;
end;

procedure TfrmTable.cmdCloseClick(Sender: TObject);
begin
  tools.ButtonPress := 0;
  Close;
end;

procedure TfrmTable.cmdInsertClick(Sender: TObject);
begin
  if not IsNumberX(lblRows.Text) then
  begin
    ShowErr('行数');
    lblRows.SetFocus;
  end
  else if not IsNumberX(lblCells.Text) then
  begin
    ShowErr('列数');
    lblCells.SetFocus;
  end
  else
  begin
    tools.ButtonPress := 1;
    MakeTableCode();
    Close;
  end;
end;

end.
