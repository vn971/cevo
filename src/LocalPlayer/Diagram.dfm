object DiaDlg: TDiaDlg
  Left = 649
  Top = 187
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 387
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object CloseBtn: TButtonB
    Left = 380
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = CloseBtnClick
    ButtonIndex = 0
  end
  object ToggleBtn: TButtonB
    Left = 13
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = ToggleBtnClick
    ButtonIndex = 15
  end
  object Popup: TPopupMenu
    Left = 16
    Top = 48
  end
end
