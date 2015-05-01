unit ButtonB;

interface

uses
  ButtonBase,
  LCLIntf,
  Classes, Graphics;

type
  TButtonB = class(TButtonBase)
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
  RegisterComponents('Samples', [TButtonB]);
end;

constructor TButtonB.Create;
begin
  inherited Create(aOwner);
  ShowHint := True;
  SetBounds(0, 0, 25, 25);
end;

procedure TButtonB.Paint;
begin
  with Canvas do
    if FGraphic <> nil then
    begin
      BitBltTransparent(Canvas, 0, 0, 25, 25, 169, 243 + 26 * byte(FDown), FGraphic);
      if FIndex >= 0 then
      begin
        BitBltTransparent(Canvas, 0, 0, 25, 25,
          1 + FIndex mod 12 * 26, 337 + FIndex div 12 * 26, FGraphic);
      end;
    end
    else
    begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 25, 25));
    end;
end;

procedure TButtonB.SetIndex(x: integer);
begin
  if x <> FIndex then
  begin
    FIndex := x;
    Invalidate;
  end;
end;

end.
