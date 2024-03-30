unit QFRichEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls, Tools,SetFontSize,SetFontColor,SetTable,SetImage,
  SetHyeperLink,SetFont,
  StrUtils, Clipbrd, LazFileUtils, LCLIntf,ComponentEditors,lazutf8;

type

  { TQFRichEditor }

  //TQFRichEditorCE = class(TComponentEditor)

  TQFRichEditor = class(TForm)//(TComponentEditorDesigner)//TComponent)
    Bevel1: TBevel;
    cmdH2: TToolButton;
    cmdHLine1: TToolButton;
    cmdHyperlink: TToolButton;
    FindDialog1: TFindDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuVStatus: TMenuItem;
    mnuOpenFFolder: TMenuItem;
    Separator14: TMenuItem;
    mnuIndent: TMenuItem;
    mnuVToolbar: TMenuItem;
    mnuCopyAppend: TMenuItem;
    mnuCutAppend: TMenuItem;
    mnuDelete: TMenuItem;
    mnuInsertFile: TMenuItem;
    Separator13: TMenuItem;
    mnuWordwrap: TMenuItem;
    mnuView: TMenuItem;
    mnuHighlight: TMenuItem;
    Separator12: TMenuItem;
    mnuSubscript: TMenuItem;
    mnuSuperscript: TMenuItem;
    mnuNewWnd: TMenuItem;
    mnuGoto: TMenuItem;
    mnuFindNext: TMenuItem;
    mnuFind: TMenuItem;
    mnuReplace: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    Separator11: TMenuItem;
    mnuSave: TMenuItem;
    Separator10: TMenuItem;
    Separator9: TMenuItem;
    mnuLowercase: TMenuItem;
    mnuUppercase: TMenuItem;
    mnuConv: TMenuItem;
    Separator8: TMenuItem;
    mnuDateTime: TMenuItem;
    mnuTable: TMenuItem;
    cmdDateTime: TToolButton;
    cmdSubscript: TToolButton;
    cmdSuperscript: TToolButton;
    cmdMark: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    RichEdit: TMemo;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuEdit: TMenuItem;
    mnuHeadings: TMenuItem;
    mnuImage: TMenuItem;
    mnuHozLine: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSaveAs: TMenuItem;
    mnuExit: TMenuItem;
    Separator7: TMenuItem;
    Separator6: TMenuItem;
    Separator5: TMenuItem;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    mnuUndo: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    mnuInsert: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuBold: TMenuItem;
    mnuItalic: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    mnuNew: TMenuItem;
    mnuFile: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    cmdNew: TToolButton;
    ToolButton10: TToolButton;
    cmdBold: TToolButton;
    cmdItalic: TToolButton;
    cmdStrike: TToolButton;
    ToolButton14: TToolButton;
    cmdOpen: TToolButton;
    cmdTable: TToolButton;
    cmdImg: TToolButton;
    ToolButton23: TToolButton;
    cmdHLine: TToolButton;
    cmdSave: TToolButton;
    ToolButton4: TToolButton;
    cmdUndo: TToolButton;
    ToolButton7: TToolButton;
    cmdH1: TToolButton;
    procedure cmdBoldClick(Sender: TObject);
    procedure cmdBoldItlicClick(Sender: TObject);
    procedure cmdCodeClick(Sender: TObject);
    procedure cmdDateTimeClick(Sender: TObject);
    procedure cmdH1Click(Sender: TObject);
    procedure cmdH2Click(Sender: TObject);
    procedure cmdHLine1Click(Sender: TObject);
    procedure cmdHLineClick(Sender: TObject);
    procedure cmdHyperlinkClick(Sender: TObject);
    procedure cmdImgClick(Sender: TObject);
    procedure cmdItalicClick(Sender: TObject);
    procedure cmdMarkClick(Sender: TObject);
    procedure cmdNewClick(Sender: TObject);
    procedure cmdOListClick(Sender: TObject);
    procedure cmdOpenClick(Sender: TObject);
    procedure cmdQuoteClick(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure cmdStrikeClick(Sender: TObject);
    procedure cmdSubscriptClick(Sender: TObject);
    procedure cmdSuperscriptClick(Sender: TObject);
    procedure cmdTableClick(Sender: TObject);
    procedure cmdUndoClick(Sender: TObject);
    procedure cmdUOListClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure mnuBoldItalicClick(Sender: TObject);
    procedure mnuCopyAppendClick(Sender: TObject);
    procedure mnuCutAppendClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuFindClick(Sender: TObject);
    procedure mnuGotoClick(Sender: TObject);
    procedure mnuHeadingsClick(Sender: TObject);
    procedure mnuHighlightClick(Sender: TObject);
    procedure mnuIndentClick(Sender: TObject);
    procedure mnuInlineCodeClick(Sender: TObject);
    procedure mnuBoldClick(Sender: TObject);
    procedure mnuBQuoteClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuDateTimeClick(Sender: TObject);
    procedure mnuH2Click(Sender: TObject);
    procedure mnuH3Click(Sender: TObject);
    procedure mnuH4Click(Sender: TObject);
    procedure mnuH5Click(Sender: TObject);
    procedure mnuH6Click(Sender: TObject);
    procedure mnuHozLineClick(Sender: TObject);
    procedure mnuHyperlinkClick(Sender: TObject);
    procedure mnuImageClick(Sender: TObject);
    procedure mnuInsertFileClick(Sender: TObject);
    procedure mnuItalicClick(Sender: TObject);
    procedure mnuLowercaseClick(Sender: TObject);
    procedure mnuNewWndClick(Sender: TObject);
    procedure mnuOListClick(Sender: TObject);
    procedure mnuOpenFFolderClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuReplaceClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuStrikeClick(Sender: TObject);
    procedure mnuSubscriptClick(Sender: TObject);
    procedure mnuSuperscriptClick(Sender: TObject);
    procedure mnuTableClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuUOListClick(Sender: TObject);
    procedure mnuUppercaseClick(Sender: TObject);
    procedure mnuVStatusClick(Sender: TObject);
    procedure mnuVToolbarClick(Sender: TObject);
    procedure mnuWordwrapClick(Sender: TObject);
    procedure RichEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure RichEditChange(Sender: TObject);
    procedure RichEditKeyPress(Sender: TObject; var Key: char);
    procedure RichEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    procedure MDStyle(Insert: string);
    procedure MDTag(Insert: string);
    procedure MDHeader(Count: integer);
    procedure MDList(Ordered: boolean);
    procedure MDDoCase(Upper: boolean);
    procedure HandleClickItem(Sender: TObject);
    procedure HandleCustomClick(Sender: TObject);
    procedure LoadTemplates;
    procedure CustomMenuItem;
    procedure CannotFindText(FindText:string);
    procedure UpdateStatusBar;
    procedure DoOpenDocument;
    procedure DoOpenDocFromFileDrop(Filename: string);
    procedure FileReadIonlyMsg;

  const
    dlgFilter = 'QF富文本文件(*.QF)|*.QF|文本文件(*.txt)|*.txt|所有文件(*.*)|*.*';
  const
    DocChanged = '文档已更改，是否保存已变更的文档?';
  const
    DefaultDateTimeFrmt = 'yyyy-mm-dd hh:mm:ss';
  public
    //constructor Create(AOwner: TComponent);
    //destructor Destroy;
  end;

var
  RichEdit: TQFRichEditor;
  TemplatePath: string;
  FindStr: string;
  DlgExecuteOK: boolean;
  m_OpenFile: string;

implementation

{$R *.lfm}

{ TQFRichEditor }

//constructor TQFRichEditor.Create(AOwner: TComponent);
//begin
  //inherited Create(AOwner);

  //RichEdit:=TMemo.Create(AOwner);
  //AOwner:=Parent;
   //Parent :=TWinControl(aOwner);

//end;

//destructor TQFRichEditor.Destroy;
//begin
  //RichEdit.Free;

  //inherited Destroy;
//end;

procedure TQFRichEditor.FileReadIonlyMsg;
begin
  MessageDlg('', '注意：已打开的文件名标记为只读.' +
    sLineBreak + sLineBreak + '保存之前可能需要更改属性.',
    mtInformation, [mbOK], '');
end;

procedure TQFRichEditor.DoOpenDocFromFileDrop(Filename: string);
begin
  m_OpenFile := Filename;

  if FileIsReadOnlyUTF8(m_OpenFile) then
  begin
    FileReadIonlyMsg;
  end;
  RichEdit.Lines.LoadFromFile(m_OpenFile);
  RichEdit.Modified := False;
end;

procedure TQFRichEditor.DoOpenDocument;
var
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(nil);
  od.Title := '打开';
  od.Filter := dlgFilter;

  if od.Execute then
  begin
    m_OpenFile := od.FileName;

    if FileIsReadOnlyUTF8(m_OpenFile) then
    begin
      FileReadIonlyMsg;
    end;

    RichEdit.Lines.LoadFromFile(m_OpenFile);
    RichEdit.Modified := False;
  end;
  od.Free;
end;

procedure TQFRichEditor.UpdateStatusBar;
begin
  StatusBar1.Panels[0].Text :=
    '  行 ' + IntToStr(RichEdit.CaretPos.Y + 1) + ', 列 ' + IntToStr(RichEdit.CaretPos.X);

  if RichEdit.Modified then
  begin
    StatusBar1.Panels[1].Text := '改';
  end
  else
  begin
    StatusBar1.Panels[1].Text := '';
  end;
end;

procedure TQFRichEditor.CannotFindText(FindText:string);
begin
  MessageDlg('', '没找到 "' + FindStr + '"',
    mtInformation, [mbOK], 0);
end;

function SearchMemo(Memo: TMemo; const SearchString: string;
  Options: TFindOptions): boolean;
var
  Buffer, P: PChar;
  Size: word;
  str:string;
begin
  Result := False;
  if Length(SearchString) = 0 then
    Exit;

  //Size := Memo.GetTextLen;
  //
  //if (Size = 0) then
  //  Exit;
  //
  //Buffer := SysUtils.StrAlloc(Size + 1);
  //
  //try
  //  Memo.GetTextBuf(Buffer, Size + 1);
  //
  //  if frDown in Options then
  //    P := SearchBuf(Buffer, Size, Memo.SelStart, Memo.SelLength, SearchString, [soDown])
  //  else
  //    P := SearchBuf(Buffer, Size, Memo.SelStart, Memo.SelLength, SearchString, []);
  //
  //  if (frMatchCase in Options) then
  //    P := SearchBuf(Buffer, Size, Memo.SelStart, Memo.SelLength,
  //      SearchString, [soMatchCase]);
  //
  //  if (frWholeWord in Options) then
  //
  //    P := SearchBuf(Buffer, Size, Memo.SelStart, Memo.SelLength,
  //      SearchString, [soWholeWord]);
  //
  //  if P <> nil then
  //  begin
  //    //Memo.Lines.IndexOf(SearchString);
  //    Memo.SelStart := P - Buffer;
  //    Memo.SelLength := utf8Length(SearchString);
  //    Result := True;
  //  end;
  //finally
  //  SysUtils.StrDispose(Buffer);
  //end;
  str:=Memo.SelText;
  if UTF8Pos(SearchString, Memo.Text)>0 then
  begin
    Memo.SelStart := UTF8Pos(SearchString, memo.Text)-1;
    Memo.SelLength := utf8Length(SearchString);
    Result := True;
  end;
  Memo.SetFocus;
end;

procedure TQFRichEditor.HandleCustomClick(Sender: TObject);
var
  S: string;
begin
  S := Trim(InputBox('定制专区', '输入选择文本', ''));
  if Length(S) > 0 then
  begin
    RichEdit.SelText := '## ' + S + sLineBreak;
  end;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.CustomMenuItem;
begin
end;

procedure TQFRichEditor.HandleClickItem(Sender: TObject);
var
  mi: TMenuItem;
  lzFile: string;
  sl: TStringList;
begin
  mi := TMenuItem(Sender);

  lzFile := TemplatePath + mi.Caption + '.txt';
  //Check the file is here.

  if FileExistsUTF8(lzFile) then
  begin
    sl := TStringList.Create;
    sl.LoadFromFile(lzFile);
    RichEdit.SelText := sl.GetText;
  end;
  RichEdit.SetFocus;
  sl.Free;
end;

procedure Split(Delimiter: char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

procedure TQFRichEditor.LoadTemplates;
begin
end;

procedure TQFRichEditor.MDDoCase(Upper: boolean);
begin
  if RichEdit.SelLength > 0 then
  begin
    if Upper then
    begin
      RichEdit.SelText := Uppercase(RichEdit.SelText);
    end
    else
    begin
      RichEdit.SelText := Lowercase(RichEdit.SelText);
    end;
  end;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.MDList(Ordered: boolean);
begin
end;

procedure TQFRichEditor.MDHeader(Count: integer);
var
  X: integer;
  S: string;
begin
  S := '';

  for X := 1 to Count do
  begin
    S := S + '#';
  end;

  RichEdit.SelText := S + ' ' + RichEdit.SelText;
  S := '';
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.MDTag(Insert: string);
var
  S0: string;
  sEnd: string;
begin
  S0 := RichEdit.SelText;

  sEnd := '</' + Insert + '>';

  RichEdit.SelText := '<' + Insert + '>' + S0 + sEnd;

  if Length(S0) = 0 then
  begin
    RichEdit.SelStart := RichEdit.SelStart - Length(sEnd);
  end;

  RichEdit.SetFocus;
  S0 := '';
  sEnd := '';
end;

procedure TQFRichEditor.MDStyle(Insert: string);
var
  S0: string;
begin
  S0 := RichEdit.SelText;
  RichEdit.SelText := Insert + S0 + Insert;

  if Length(S0) = 0 then
  begin
    RichEdit.SelStart := RichEdit.SelStart - Length(Insert);
  end;

  RichEdit.SetFocus;
end;

procedure TQFRichEditor.FormCreate(Sender: TObject);
var
  sCmd: string;
begin
  DlgExecuteOK := False;
  //mnuAbout.Caption := '&关于 ' + Caption + '...';
  //App data path
  Data_Path := ExtractFilePath(Application.ExeName) + 'data\';

  //Template path
  TemplatePath := Data_Path + 'tpl\';
  //Create custom menu
  CustomMenuItem;
  //Load the section templates
  if DirectoryExistsUTF8(TemplatePath) then
  begin
    LoadTemplates;
  end;
  //Check for opening file from command line
  if paramCount > 0 then
  begin
    sCmd := ParamStr(1);
    if FileExistsUTF8(sCmd) then
    begin
      m_OpenFile := sCmd;

      if FileIsReadOnlyUTF8(m_OpenFile) then
      begin
        FileReadIonlyMsg;
      end;

      RichEdit.Lines.LoadFromFile(m_OpenFile);
      RichEdit.Modified := False;
    end;
  end;
  RichEditChange(Sender);
end;

procedure TQFRichEditor.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  lzFile: string;
begin
  lzFile := FileNames[0];

  if RichEdit.Modified then
  begin
    case MessageDlg('', DocChanged, mtInformation, [mbYes, mbNo, mbCancel], '') of
      mrNo:
      begin
        DoOpenDocFromFileDrop(lzFile);
      end;
      mrYes:
      begin
        if FileExistsUTF8(m_OpenFile) then
        begin
          RichEdit.Lines.SaveToFile(m_OpenFile);
          DoOpenDocFromFileDrop(lzFile);
        end
        else
        begin
          mnuSaveAsClick(Sender);
          if DlgExecuteOK then
          begin
            DoOpenDocFromFileDrop(lzFile);
          end;
        end;
      end;
    end;
  end
  else
  begin
    DoOpenDocFromFileDrop(lzFile);
  end;

  UpdateStatusBar;
  RichEdit.SetFocus;

end;

procedure TQFRichEditor.mnuBoldItalicClick(Sender: TObject);
begin
  MDStyle('***');
end;

procedure TQFRichEditor.mnuCopyAppendClick(Sender: TObject);
var
  sOld: string;
begin
  sOld := clipboard.AsText;
  clipboard.AsText := sOld + RichEdit.SelText;
end;

procedure TQFRichEditor.mnuCutAppendClick(Sender: TObject);
var
  sOld: string;
begin
  sOld := clipboard.AsText;
  clipboard.AsText := sOld + RichEdit.SelText;
  RichEdit.SelText := '';
end;

procedure TQFRichEditor.mnuDeleteClick(Sender: TObject);
begin
  RichEdit.SelText := '';
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuFindClick(Sender: TObject);
begin
  FindDialog1.FindText := RichEdit.SelText;
  FindDialog1.Execute;
end;

procedure TQFRichEditor.mnuGotoClick(Sender: TObject);
var
  nLine: integer;
  NewVal: string;
  sLineLn: integer;
begin

  NewVal := IntToStr(RichEdit.CaretPos.y);

  if NewVal = '0' then
    NewVal := '1';

  NewVal := InputBox('转到', '行数:', NewVal);
  if TryStrToInt(NewVal, nLine) then
  begin
    if nLine > RichEdit.Lines.Count then
      nLine := RichEdit.Lines.Count
    else if nLine <= 0 then
    begin
      nLine := 1;
    end;
    sLineLn := Length(RichEdit.Lines[nLine - 1]);
    RichEdit.CaretPos := TPoint.Create(0, nLine - 1);
    RichEdit.SelLength := sLineLn;
  end
  else
  begin
    MessageDlg('', '值不是正确的整数.',
      mtError, [mbOK], '');
  end;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuHeadingsClick(Sender: TObject);
var sfz:TSetFontSizeFrm;
begin
  tools.ButtonPress := 0;
  sfz:=TSetFontSizeFrm.Create(nil);
  sfz.ShowModal;

  if tools.ButtonPress = 1 then
  begin
    RichEdit.SelText := tools.fontsize;
  end;
  sfz.Free;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuHighlightClick(Sender: TObject);
var sfz:TSetFontColorFrm;
begin
  tools.ButtonPress := 0;
  tools.SelTextlength:=length(RichEdit.SelText);
  sfz:=TSetFontColorFrm.Create(nil);
  sfz.ShowModal;

  if tools.ButtonPress = 1 then
  begin
    if tools.SelTextlength=0 then
      RichEdit.SelText := tools.fontcolor
    else
      RichEdit.SelText := tools.fontcolor+RichEdit.SelText+'</c>';
  end;
  sfz.Free;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuIndentClick(Sender: TObject);
begin
  mnuIndent.Checked := not mnuIndent.Checked;
end;

procedure TQFRichEditor.mnuInlineCodeClick(Sender: TObject);
begin
  if Pos('`', RichEdit.Text) > 0 then
  begin
    MDStyle('``');
  end
  else
  begin
    MDStyle('`');
  end;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.cmdNewClick(Sender: TObject);
begin
  mnuNewClick(Sender);
end;

procedure TQFRichEditor.cmdOListClick(Sender: TObject);
begin
  mnuOListClick(Sender);
end;

procedure TQFRichEditor.cmdH1Click(Sender: TObject);
begin
  mnuHeadingsClick(Sender);
end;

procedure TQFRichEditor.cmdBoldClick(Sender: TObject);
begin
  mnuBoldClick(Sender);
end;

procedure TQFRichEditor.cmdBoldItlicClick(Sender: TObject);
begin
  mnuBoldItalicClick(Sender);
end;

procedure TQFRichEditor.cmdCodeClick(Sender: TObject);
begin
  mnuInlineCodeClick(Sender);
end;

procedure TQFRichEditor.cmdDateTimeClick(Sender: TObject);
begin
  mnuDateTimeClick(Sender);
end;

procedure TQFRichEditor.cmdH2Click(Sender: TObject);
begin
  mnuH2Click(Sender);
end;

procedure TQFRichEditor.cmdHLine1Click(Sender: TObject);
begin
  RichEdit.SelText := sLineBreak + '[2LINE]' + sLineBreak;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.cmdHLineClick(Sender: TObject);
begin
  RichEdit.SelText := sLineBreak + '[LINE]' + sLineBreak;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.cmdHyperlinkClick(Sender: TObject);
begin
  mnuHyperlinkClick(Sender);
end;

procedure TQFRichEditor.cmdImgClick(Sender: TObject);
begin
  mnuImageClick(Sender);
end;

procedure TQFRichEditor.cmdItalicClick(Sender: TObject);
begin
  mnuItalicClick(Sender);
end;

procedure TQFRichEditor.cmdMarkClick(Sender: TObject);
begin
  mnuHighlightClick(Sender);
end;

procedure TQFRichEditor.cmdOpenClick(Sender: TObject);
begin
  mnuOpenClick(Sender);
end;

procedure TQFRichEditor.cmdQuoteClick(Sender: TObject);
begin
  mnuBQuoteClick(Sender);
end;

procedure TQFRichEditor.cmdSaveClick(Sender: TObject);
begin
  mnuSaveClick(Sender);
end;

procedure TQFRichEditor.cmdStrikeClick(Sender: TObject);
var
  S0: string;
begin
  S0 := RichEdit.SelText;
  if trim(s0)<>'' then
    RichEdit.SelText := '<@>' + S0 + '</>'
  else
    RichEdit.SelText := '[@]' + S0  ;

  RichEdit.SetFocus;
end;

procedure TQFRichEditor.cmdSubscriptClick(Sender: TObject);
begin
  mnuSubscriptClick(Sender);
end;

procedure TQFRichEditor.cmdSuperscriptClick(Sender: TObject);
begin
  mnuSuperscriptClick(Sender);
end;

procedure TQFRichEditor.cmdTableClick(Sender: TObject);
begin
  mnuTableClick(Sender);
end;

procedure TQFRichEditor.cmdUndoClick(Sender: TObject);
begin
  mnuUndoClick(Sender);
end;

procedure TQFRichEditor.cmdUOListClick(Sender: TObject);
begin
  mnuUOListClick(Sender);
end;

procedure TQFRichEditor.FindDialog1Find(Sender: TObject);
begin
  with Sender as TFindDialog do
  begin
    FindStr := FindText;
    if not SearchMemo(RichEdit, FindText, Options) then
    begin
      CannotFindText(FindStr);
    end;
  end;
end;

procedure TQFRichEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caNone;

  if RichEdit.Modified then
  begin
    case MessageDlg('', DocChanged, mtInformation, [mbYes, mbNo, mbCancel], '') of
      mrNo:
      begin
        CloseAction := caFree;
      end;
      mrYes:
      begin
        if FileExistsUTF8(m_OpenFile) then
        begin
          RichEdit.Lines.SaveToFile(m_OpenFile);
          CloseAction := caFree;
        end
        else
        begin
          mnuSaveAsClick(Sender);
          if DlgExecuteOK then
          begin
            CloseAction := caFree;
          end;
        end;
      end;
    end;
  end
  else
  begin
    CloseAction := caFree;
  end;

end;

procedure TQFRichEditor.mnuBoldClick(Sender: TObject);
begin
  if trim(RichEdit.SelText)<>'' then
    RichEdit.SelText := '<#>' + RichEdit.SelText + '</>'
  else
    RichEdit.SelText := '[#]' + RichEdit.SelText ;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuBQuoteClick(Sender: TObject);
var
  S: string;
  lst: TStringList;
  I: integer;
  S0: string;
begin
  S0 := '';
  lst := TStringList.Create;
  S := RichEdit.SelText;
  Split(#13, S, lst);
  for I := 0 to lst.Count - 1 do
  begin
    S0 := S0 + '> ' + Trim(lst[I]) + sLineBreak;
  end;

  if Length(S0) = 0 then
  begin
    RichEdit.SelText := '> ';
  end
  else
  begin
    RichEdit.SelText := Trim(S0);
  end;

  S0 := '';
  S := '';
  lst.Free;

end;

procedure TQFRichEditor.mnuCopyClick(Sender: TObject);
begin
  RichEdit.CopyToClipboard;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuDateTimeClick(Sender: TObject);
var
  sFrmt: string;
  NoCancel: boolean;
begin
  sFrmt := DefaultDateTimeFrmt;
  NoCancel := InputQuery('Date / Time', '格式', sFrmt);

  if NoCancel then
  begin
    RichEdit.SelText := FormatDateTime(sFrmt, Now);
  end;

  RichEdit.SetFocus;
  sFrmt := '';
end;

procedure TQFRichEditor.mnuH2Click(Sender: TObject);
var sfz:TSetFontFrm;
begin
  tools.ButtonPress := 0;
  sfz:=TSetFontFrm.Create(nil);
  sfz.ShowModal;

  if tools.ButtonPress = 1 then
  begin
    if tools.FontName<>'' then
      RichEdit.SelText :='<Font='+tools.FontName+'>'+RichEdit.SelText+'</Font>';
  end;
  sfz.Free;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuH3Click(Sender: TObject);
begin
  MDHeader(3);
end;

procedure TQFRichEditor.mnuH4Click(Sender: TObject);
begin
  MDHeader(4);
end;

procedure TQFRichEditor.mnuH5Click(Sender: TObject);
begin
  MDHeader(5);
end;

procedure TQFRichEditor.mnuH6Click(Sender: TObject);
begin
  MDHeader(6);
end;

procedure TQFRichEditor.mnuHozLineClick(Sender: TObject);
begin
  RichEdit.SelText := sLineBreak + '----' + sLineBreak;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuHyperlinkClick(Sender: TObject);
var
  frm: TfrmHyeperLink;
begin
  frm := TfrmHyeperLink.Create(nil);
  Tools.ButtonPress := 0;
  frm.ShowModal;

  if Tools.ButtonPress = 1 then
  begin
    if trim(Tools.LinkDesc)<>'' then
      RichEdit.SelText := '<hlk>(' + Tools.LinkDesc+')'+ Tools.LinkUrl + '</hlk>'
    else
      RichEdit.SelText := '<hlk>' + Tools.LinkUrl + '</hlk>';
  end;

  RichEdit.SetFocus;
  frm.Free;
end;

procedure TQFRichEditor.mnuImageClick(Sender: TObject);
var
  frm: TfrmImage;
begin
  frm := TfrmImage.Create(nil);
  Tools.ButtonPress := 0;
  frm.ShowModal;

  if Tools.ButtonPress = 1 then
  begin
    RichEdit.SelText :=sLineBreak + '[img]'+ Tools.ImgFile + sLineBreak;
  end;

  RichEdit.SetFocus;
  frm.Free;
end;

procedure TQFRichEditor.mnuInsertFileClick(Sender: TObject);
var
  od: TOpenDialog;
  sl: TStringList;
begin
  od := TOpenDialog.Create(nil);
  sl := TStringList.Create;

  od.Title := '插入文件';
  od.Filter := dlgFilter;
  od.FilterIndex := 1;

  if od.Execute then
  begin
    sl.LoadFromFile(od.FileName);
    RichEdit.SelText := sl.GetText;
    sl.Free;
  end;
  od.Free;
end;

procedure TQFRichEditor.mnuItalicClick(Sender: TObject);
var
  S0: string;
begin
  S0 := RichEdit.SelText;
  if trim(s0)<>'' then
    RichEdit.SelText := '<$>' + S0 + '</>'
  else
    RichEdit.SelText := '[$]' + S0;

  if Length(S0) = 0 then
  begin
    RichEdit.SelStart := RichEdit.SelStart - Length('<$></>');
  end;

  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuLowercaseClick(Sender: TObject);
begin
  MDDoCase(False);
end;

procedure TQFRichEditor.mnuNewWndClick(Sender: TObject);
begin
  //Start a new process of the app
  OpenDocument(Application.ExeName);
end;

procedure TQFRichEditor.mnuOListClick(Sender: TObject);
begin
  MDList(True);
end;

procedure TQFRichEditor.mnuOpenFFolderClick(Sender: TObject);
begin
  if FileExistsUTF8(m_OpenFile) then
  begin
    OpenDocument(ExtractFileDir(m_OpenFile));
  end;
end;

procedure TQFRichEditor.mnuPasteClick(Sender: TObject);
begin
  RichEdit.PasteFromClipboard;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuReplaceClick(Sender: TObject);
begin
  ReplaceDialog1.FindText := FindStr;
  ReplaceDialog1.Execute;
end;

procedure TQFRichEditor.mnuSaveAsClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(nil);
  sd.Title := 'Save As';
  sd.FilterIndex := 1;
  sd.Filter := dlgFilter;
  sd.DefaultExt := 'QF';

  if m_OpenFile <> '' then
  begin
    sd.FileName := ExtractFileName(m_OpenFile);
  end;

  DlgExecuteOK := sd.Execute;

  if DlgExecuteOK then
  begin

    if FileExistsUTF8(sd.FileName) then
    begin
      if MessageDlg('',
        '文件名已存在，是否要覆盖该文件?',
        mtInformation, [mbYes, mbNo, mbCancel], '') = mrYes then
      begin
        m_OpenFile := sd.FileName;
        RichEdit.Lines.SaveToFile(sd.FileName);
      end;
    end
    else
    begin
      m_OpenFile := sd.FileName;
      RichEdit.Lines.SaveToFile(sd.FileName);
    end;
    RichEdit.Modified := False;
  end;
  sd.Free;
  RichEdit.SetFocus;
  UpdateStatusBar;
end;

procedure TQFRichEditor.mnuSelectAllClick(Sender: TObject);
begin
  RichEdit.SelectAll;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuStrikeClick(Sender: TObject);
begin
  MDStyle('~~');
end;

procedure TQFRichEditor.mnuSubscriptClick(Sender: TObject);
begin
  MDTag('sub');
end;

procedure TQFRichEditor.mnuSuperscriptClick(Sender: TObject);
begin
  MDTag('sup');
end;

procedure TQFRichEditor.mnuTableClick(Sender: TObject);
var ft:TfrmTable;
begin
  tools.ButtonPress := 0;
  ft:=TfrmTable.Create(nil);
  ft.ShowModal;
  if tools.ButtonPress = 1 then
  begin
    RichEdit.SelText := tools.TableCode;
  end;
  ft.Free;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuUndoClick(Sender: TObject);
begin
  RichEdit.Undo;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuCutClick(Sender: TObject);
begin
  RichEdit.CutToClipboard;
  RichEdit.SetFocus;
end;

procedure TQFRichEditor.mnuExitClick(Sender: TObject);
begin
  //Close;
end;

procedure TQFRichEditor.mnuNewClick(Sender: TObject);
begin

  if RichEdit.Modified then
  begin
    case MessageDlg('', DocChanged, mtInformation, [mbYes, mbNo, mbCancel], '') of

      mrNo:
      begin
        RichEdit.Clear;
        RichEdit.Modified := False;
        m_OpenFile := '';
      end;
      mrYes:
      begin

        if FileExistsUTF8(m_OpenFile) then
        begin
          RichEdit.Lines.SaveToFile(m_OpenFile);
          RichEdit.Clear;
          RichEdit.Modified := False;
          m_OpenFile := '';
        end
        else
        begin
          mnuSaveAsClick(Sender);
          if DlgExecuteOK then
          begin
            RichEdit.Clear;
            RichEdit.Modified := False;
            m_OpenFile := '';
          end;
        end;
      end;
    end;
  end
  else
  begin
    RichEdit.Clear;
    m_OpenFile := '';
  end;

  UpdateStatusBar;

end;

procedure TQFRichEditor.mnuOpenClick(Sender: TObject);
begin

  if RichEdit.Modified then
  begin
    case MessageDlg('', DocChanged, mtInformation, [mbYes, mbNo, mbCancel], '') of
      mrNo:
      begin
        DoOpenDocument;
      end;
      mrYes:
      begin
        if FileExistsUTF8(m_OpenFile) then
        begin
          RichEdit.Lines.SaveToFile(m_OpenFile);
          DoOpenDocument;
        end
        else
        begin
          mnuSaveAsClick(Sender);
          if DlgExecuteOK then
          begin
            DoOpenDocument;
          end;
        end;
      end;
    end;
  end
  else
  begin
    DoOpenDocument;
  end;

  UpdateStatusBar;
  RichEdit.SetFocus;

end;

procedure TQFRichEditor.mnuSaveClick(Sender: TObject);
begin
  RichEdit.WordWrap:=false;
  if FileExists(m_OpenFile) then
  begin
    RichEdit.Lines.SaveToFile(m_OpenFile);
    RichEdit.Modified := False;
  end
  else
  begin
    mnuSaveAsClick(Sender);
  end;
  RichEdit.WordWrap:=mnuWordwrap.Checked;
  UpdateStatusBar;
end;

procedure TQFRichEditor.mnuUOListClick(Sender: TObject);
begin
  MDList(False);
end;

procedure TQFRichEditor.mnuUppercaseClick(Sender: TObject);
begin
  MDDoCase(True);
end;

procedure TQFRichEditor.mnuVStatusClick(Sender: TObject);
begin
  mnuVStatus.Checked := not mnuVStatus.Checked;
  StatusBar1.Visible := mnuVStatus.Checked;
end;

procedure TQFRichEditor.mnuVToolbarClick(Sender: TObject);
begin
  mnuVToolbar.Checked := not mnuVToolbar.Checked;
  ToolBar1.Visible := mnuVToolbar.Checked;
end;

procedure TQFRichEditor.mnuWordwrapClick(Sender: TObject);
begin
  mnuWordwrap.Checked := not mnuWordwrap.Checked;
  RichEdit.WordWrap := mnuWordwrap.Checked;
end;

procedure TQFRichEditor.RichEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 // QFRichEditorChange(Sender);
end;

procedure TQFRichEditor.ReplaceDialog1Find(Sender: TObject);
begin
  with Sender as TReplaceDialog do
    if not SearchMemo(RichEdit, FindText, Options) then
    begin
      CannotFindText(FindText);
    end;
end;

procedure TQFRichEditor.ReplaceDialog1Replace(Sender: TObject);
var
  Found: boolean;
begin
  with ReplaceDialog1 do

  begin
    { Replace }
    if (frReplace in Options) and (RichEdit.SelText = FindText) then
      RichEdit.SelText := ReplaceText;

    Found := SearchMemo(RichEdit, FindText, Options);

    { Replace All }
    if (frReplaceAll in Options) then
    begin
      RichEdit.SelStart := 0;
      while Found do
      begin
        if (RichEdit.SelText = FindText) then
          RichEdit.SelText := ReplaceText;
        Found := SearchMemo(RichEdit, FindText, Options);
      end;
    end;

    if (not Found) and (frReplace in Options) then
    begin
      CannotFindText(FindText);
    end;
  end;
end;

procedure TQFRichEditor.RichEditChange(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TQFRichEditor.RichEditKeyPress(Sender: TObject; var Key: char);
var
  line, col, indent: integer;
  S: string;
begin
  if key in ['(', '[', '{'] then
  begin
    if key = '(' then RichEdit.SelText := '('+RichEdit.SelText+')';
    if key = '[' then RichEdit.SelText := '['+RichEdit.SelText+']';
    if key = '{' then RichEdit.SelText := '{'+RichEdit.SelText+'}';
    RichEdit.SelStart := RichEdit.SelStart - 1;
    key := #0;
  end;

  if key = #13 then
  begin
    key := #0;
    with Sender as TMemo do
    begin

      if mnuIndent.Checked then
      begin
        line := CaretPos.Y;
        Col := CaretPos.X;

        S := system.Copy(Lines[line], 1, col);

        indent := 0;
        while (indent < length(S)) and (S[indent + 1] in [#32, #9]) do
          Inc(indent);

        SelText := sLineBreak + system.Copy(S, 1, indent);
        S := '';
      end
      else
      begin
        Key := #13;
      end;
    end;
  end;
end;

procedure TQFRichEditor.RichEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  RichEditChange(Sender);
end;

//initialization
//  RegisterClasses([TQFRichEditor]);


end.
