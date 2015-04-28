unit ButtonC;

interface

uses
  ButtonBase,
  LCLIntf, LCLType,
  Classes, Graphics;

type
  TButtonC = class(TButtonBase)
    constructor Create(aOwner: TComponent); override;
  private
    FIndex: integer;
    procedure SetIndex(x: integer);
  published
    property Visible;
    property ButtonIndex: integer read FIndex write SetIndex;
    property OnClick;
  protected
    procedure Paint; override;
  end;

procedure Register;

implementation
uses
  screentools;

procedure Register;
begin
  RegisterComponents('Samples', [TButtonC]);
end;

constructor TButtonC.Create;
begin
  inherited Create(aOwner);
  ShowHint := True;
  SetBounds(0, 0, 12, 12);
end;

procedure TButtonC.Paint;
begin
  with Canvas do
    if FGraphic <> nil then
      BitBltUgly(Canvas.Handle, 0, 0, 12, 12, FGraphic.Canvas,
        169 + 13 * byte(FDown), 159 + 13 * FIndex, SRCCOPY)
    else
    begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 12, 12));
    end;
end;

procedure TButtonC.SetIndex(x: integer);
begin
  if x <> FIndex then
  begin
    FIndex := x;
    Invalidate;
  end;
end;

end.
