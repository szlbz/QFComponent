unit SetImage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ExtDlgs, Tools;

type

  { TfrmImage }

  TfrmImage = class(TForm)
    Button1: TButton;
    cmdClose: TButton;
    cmdInsert: TButton;
    lblImgUrl: TLabeledEdit;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure Button1Click(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdInsertClick(Sender: TObject);
  private

  public

  end;

var
  frmImage: TfrmImage;

implementation

{$R *.lfm}

{ TfrmImage }

procedure TfrmImage.cmdCloseClick(Sender: TObject);
begin
  Tools.ButtonPress := 0;
  Close;
end;

procedure TfrmImage.Button1Click(Sender: TObject);
begin
  if  OpenPictureDialog1.Execute then
  lblImgUrl.Text:= OpenPictureDialog1.FileName;
end;

procedure TfrmImage.cmdInsertClick(Sender: TObject);
begin
  Tools.ButtonPress := 1;
  Tools.ImgFile := lblImgUrl.Text;
  Close;
end;

end.
