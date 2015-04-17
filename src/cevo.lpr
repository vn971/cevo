{$INCLUDE switches.pas}

library cevo;

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  stringtables in 'stringtables.pas',
  directories in 'directories.pas',
  protocol in 'protocol.pas',
  cmdlist in 'cmdlist.pas',
  database in 'database.pas',
  gameserver in 'gameserver.pas',
  cityprocessing in 'cityprocessing.pas',
  unitprocessing in 'unitprocessing.pas',
  direct in 'direct.pas' {directdlg},
  screentools in 'screentools.pas',
  start in 'start.pas' {startdlg},
  messg in 'messg.pas' {messgdlg},
  inp in 'inp.pas' {inputdlg},
  back in 'back.pas' {background},
  log in 'log.pas' {logdlg},
  pvsb in 'localplayer\pvsb.pas',
  localplayer in 'localplayer\localplayer.pas',
  clienttools in 'localplayer\clienttools.pas',
  diplomacy in 'localplayer\diplomacy.pas',
  tribes in 'localplayer\tribes.pas',
  isoengine in 'localplayer\isoengine.pas',
  term in 'localplayer\term.pas' {mainscreen},
  messgex in 'localplayer\messgex.pas' {messgexdlg},
  basewin in 'localplayer\basewin.pas',
  help in 'localplayer\help.pas' {helpdlg},
  select in 'localplayer\select.pas' {listdlg},
  cityscreen in 'localplayer\cityscreen.pas' {citydlg},
  unitstat in 'localplayer\unitstat.pas' {unitstatdlg},
  draft in 'localplayer\draft.pas' {draftdlg},
  natstat in 'localplayer\natstat.pas' {natstatdlg},
  diagram in 'localplayer\diagram.pas' {diadlg},
  wonders in 'localplayer\wonders.pas' {wonderdlg},
  nego in 'localplayer\nego.pas' {negodlg},
  citytype in 'localplayer\citytype.pas' {citytypedlg},
  enhance in 'localplayer\enhance.pas' {enhancedlg},
  noterm in 'noterm.pas' {notermdlg},
  //sound in 'sound.pas' {soundplayer},
  battle in 'localplayer\battle.pas' {battledlg},
  rates in 'localplayer\rates.pas' {ratesdlg},
  techtree in 'localplayer\techtree.pas' {techtreedlg};

{$R *.res}

procedure Run(clientPtr: pointer); stdcall;
begin
DotNetClient:=TClientCall(clientPtr);
Application.Initialize;
Application.Title := '';
Application.CreateForm(TDirectDlg, DirectDlg);
Application.CreateForm(TStartDlg, StartDlg);
Application.CreateForm(TMessgDlg, MessgDlg);
Application.CreateForm(TInputDlg, InputDlg);
Application.CreateForm(TBackground, Background);
Application.CreateForm(TLogDlg, LogDlg);
Application.Run;
end;

exports
Run name 'Run';

end.

