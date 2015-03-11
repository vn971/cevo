program bv;

uses
  Forms,
  StatMain in 'StatMain.pas' {MainForm},
  CmdList in '..\CmdList.pas',
  Protocol in '..\Protocol.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Book Viewer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
