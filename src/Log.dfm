object LogDlg: TLogDlg
  Left = 256
  Top = 187
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Log'
  ClientHeight = 280
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 16
  object List: TMemo
    Left = 0
    Top = 0
    Width = 339
    Height = 280
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
    OnMouseDown = ListMouseDown
  end
  object LogPopup: TPopupMenu
    AutoPopup = False
    Left = 8
    Top = 8
    object mInvalid: TMenuItem
      Caption = 'Invalid Server Calls'
      OnClick = Toggle
    end
    object mNegotiation: TMenuItem
      Caption = 'Negotiation'
      OnClick = Toggle
    end
    object mTime: TMenuItem
      Caption = 'Client Handover'
      OnClick = Toggle
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mLog0: TMenuItem
      Caption = 'AI Messages Off'
      Checked = True
      RadioItem = True
      OnClick = mLogClick
    end
    object mLog1: TMenuItem
      Tag = 1
      Caption = 'AI Messages Level 1'
      RadioItem = True
      OnClick = mLogClick
    end
    object mLog2: TMenuItem
      Tag = 2
      Caption = 'AI Messages Level 1+2'
      RadioItem = True
      OnClick = mLogClick
    end
    object mLog3: TMenuItem
      Tag = 999
      Caption = 'All AI Messages'
      RadioItem = True
      OnClick = mLogClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mSlot: TMenuItem
      Caption = 'Nation <-> Slot'
      OnClick = mSlotClick
    end
    object mClear: TMenuItem
      Caption = 'Clear'
      OnClick = mClearClick
    end
  end
end
