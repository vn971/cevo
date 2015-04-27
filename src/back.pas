{$INCLUDE switches.pas}

unit back;

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Graphics, Forms;

type
  TBackground = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    img: TFPImageBitmap;
  end;

var
  Background: TBackground;

implementation

uses
  FileUtil,
  Directories, ScreenTools, Start;

{$R *.lfm}

procedure TBackground.FormShow(Sender: TObject);
begin
  if FullScreen or True then // lazarus todo: stop using this hardcoded fullscreen override
  begin
    img := LoadAnyGraphics(GraphicsDirectory + 'Background');
  end
  else
  begin
    WindowState := wsNormal;
    Width := StartDlg.Width + 16;
    Height := StartDlg.Height + 16;
    Left := StartDlg.Left - 8;
    Top := StartDlg.Top - 8;
  end;
end;

procedure TBackground.FormPaint(Sender: TObject);
begin
  if img <> nil then
    BitBltTransparent(Canvas,
      Screen.Width - img.Width - (Screen.Width - 800) * 3 div 8,
      (Screen.Height - 600) div 3,
      img.Width, img.Height, 0, 0, img);
end;

procedure TBackground.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(img);
end;

end.
