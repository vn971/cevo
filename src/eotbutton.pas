unit EOTButton;

interface

uses
  ButtonBase,
  LCLIntf, LCLType,
  Classes, Graphics;

const
  eotBlinkOff = -1;
  eotCancel = 0;
  eotGray = 1;
  eotBlinkOn = 2;
  eotBackToNego = 3;

type
  TEOTButton = class(TButtonBase)
    constructor Create(aOwner: TComponent); override;
    procedure SetButtonIndexFast(x: integer);
    procedure SetBack(ca: TCanvas; x, y: integer);
  private
    FTemplate: TFPImageBitmap;
    FIndex: integer;
    procedure SetIndex(x: integer);
  public
    destructor Destroy; override;
    property Template: TFPImageBitmap read FTemplate write FTemplate;
  published
    property Visible;
    property ButtonIndex: integer read FIndex write SetIndex;
    property OnClick;
  protected
    Buffer, Back: TPortableNetworkGraphic;
    procedure Paint; override;
  end;

procedure Register;

implementation
uses
  screentools;

procedure Register;
begin
  RegisterComponents('Samples', [TEOTButton]);
end;

procedure ImageOp_CBC(Dst, Src: TFPImageBitmap;
  xDst, yDst, xSrc, ySrc, w, h, Color0, Color2: integer);
// Src is template
// B channel = Color0 amp
// G channel = background amp (old Dst content), 128=original brightness
// R channel = Color2 amp
type
  TLine = array[0..9999, 0..2] of byte;
var
  ix, iy, amp0, amp1, trans, Value: integer;
  SrcLine, DstLine: ^TLine;
begin
  Src.BeginUpdate();
  Dst.BeginUpdate();
  for iy := 0 to h - 1 do
  begin
    SrcLine := Src.ScanLine[ySrc + iy];
    DstLine := Dst.ScanLine[yDst + iy];
    for ix := 0 to w - 1 do
    begin
      trans := SrcLine[xSrc + ix, 0] * 2; // green channel = transparency
      amp0 := SrcLine[xSrc + ix, 1] * 2;
      amp1 := SrcLine[xSrc + ix, 2] * 2;
      if trans <> $FF then
      begin
        Value := (DstLine[xDst + ix][0] * trans + (Color2 shr 16 and $FF) *
          amp1 + (Color0 shr 16 and $FF) * amp0) div $FF;
        if Value < 256 then
          DstLine[xDst + ix][0] := Value
        else
          DstLine[xDst + ix][0] := 255;
        Value := (DstLine[xDst + ix][1] * trans + (Color2 shr 8 and $FF) *
          amp1 + (Color0 shr 8 and $FF) * amp0) div $FF;
        if Value < 256 then
          DstLine[xDst + ix][1] := Value
        else
          DstLine[xDst + ix][1] := 255;
        Value := (DstLine[xDst + ix][2] * trans + (Color2 and $FF) *
          amp1 + (Color0 and $FF) * amp0) div $FF;
        if Value < 256 then
          DstLine[xDst + ix][2] := Value
        else
          DstLine[xDst + ix][2] := 255;
      end;
    end;
  end;
  Dst.EndUpdate();
  Src.EndUpdate();
end;

constructor TEOTButton.Create;
begin
  inherited Create(aOwner);
  Buffer := TPortableNetworkGraphic.Create;
  Buffer.PixelFormat := pf24bit;
  Buffer.Width := 48;
  Buffer.Height := 48;
  Back := TPortableNetworkGraphic.Create;
  Back.PixelFormat := pf24bit;
  Back.Width := 48;
  Back.Height := 48;
  ShowHint := True;
  SetBounds(0, 0, 48, 48);
end;

destructor TEOTButton.Destroy;
begin
  Buffer.Free;
  Back.Free;
  inherited Destroy;
end;

procedure TEOTButton.Paint;
begin
  with Canvas do
    if FGraphic <> nil then
    begin
      BitBltTransparent(Canvas, 0, 0, 48, 48, 0,48 * byte(FDown), endOfTurnButton);
      if FIndex >= 0 then
        BitBltTransparent(Canvas, 8, 8, 32, 32, 1 + 32 * byte(FIndex), 0, endOfTurnImages);
    end
    else
    begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 48, 48));
    end;
end;

procedure TEOTButton.SetIndex(x: integer);
begin
  if x <> FIndex then
  begin
    FIndex := x;
    Invalidate;
  end;
end;

procedure TEOTButton.SetButtonIndexFast(x: integer);
begin
  if Visible and (x <> FIndex) then
  begin
    FIndex := x;
    try
      Paint
    except
    end;
  end;
end;

procedure TEOTButton.SetBack(ca: TCanvas; x, y: integer);
begin
  BitBltUgly(Back.Canvas, 0, 0, 48, 48, ca, x, y, SRCCOPY);
end;

end.
