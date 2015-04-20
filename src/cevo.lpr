{$INCLUDE switches.pas}

program cevo;

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  back in 'back.pas' {background},
  cityprocessing in 'cityprocessing.pas',
  cmdlist in 'cmdlist.pas',
  database in 'database.pas',
  direct in 'direct.pas' {directdlg},
  directories in 'directories.pas',
  gameserver in 'gameserver.pas',
  inp in 'inp.pas' {inputdlg},
  log in 'log.pas' {logdlg},
  messg in 'messg.pas' {messgdlg},
  noterm in 'noterm.pas' {notermdlg},
  protocol in 'protocol.pas',
  pvsb in 'localplayer\pvsb.pas',
  screentools in 'screentools.pas',
  start in 'start.pas' {startdlg},
  stringtables in 'stringtables.pas',
  unitprocessing in 'unitprocessing.pas',
  basewin in 'localplayer\basewin.pas',
  battle in 'localplayer\battle.pas' {battledlg},
  cityscreen in 'localplayer\cityscreen.pas' {citydlg},
  citytype in 'localplayer\citytype.pas' {citytypedlg},
  clienttools in 'localplayer\clienttools.pas',
  diagram in 'localplayer\diagram.pas' {diadlg},
  diplomacy in 'localplayer\diplomacy.pas',
  draft in 'localplayer\draft.pas' {draftdlg},
  enhance in 'localplayer\enhance.pas' {enhancedlg},
  help in 'localplayer\help.pas' {helpdlg},
  isoengine in 'localplayer\isoengine.pas',
  localplayer in 'localplayer\localplayer.pas',
  messgex in 'localplayer\messgex.pas' {messgexdlg},
  natstat in 'localplayer\natstat.pas' {natstatdlg},
  nego in 'localplayer\nego.pas' {negodlg},
  rates in 'localplayer\rates.pas' {ratesdlg},
  select in 'localplayer\select.pas' {listdlg},
  techtree in 'localplayer\techtree.pas' {techtreedlg},
  term in 'localplayer\term.pas' {mainscreen},
  tribes in 'localplayer\tribes.pas',
  unitstat in 'localplayer\unitstat.pas' {unitstatdlg},
  wonders in 'localplayer\wonders.pas' {wonderdlg};

{$R *.res}

begin
Application.Initialize;
Application.Title := 'C-evo';
Application.CreateForm(TDirectDlg, DirectDlg);
Application.CreateForm(TStartDlg, StartDlg);
Application.CreateForm(TMessgDlg, MessgDlg);
Application.CreateForm(TInputDlg, InputDlg);
Application.CreateForm(TBackground, Background);
Application.CreateForm(TLogDlg, LogDlg);
Application.Run;
end.
