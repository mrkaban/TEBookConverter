program TEBookConverter;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  UnitMain in 'Forms\UnitMain.pas' {MainForm} ,
  UnitEncoder in 'Units\UnitEncoder.pas',
  UnitLogs in 'Forms\UnitLogs.pas' {LogForm} ,
  UnitAbout in 'Forms\UnitAbout.pas' {AboutForm} ,
  UnitInfo in 'Forms\UnitInfo.pas' {FileInfoForm} ,
  UnitFileInfo in 'Units\UnitFileInfo.pas',
  windows7taskbar in 'Units\windows7taskbar.pas',
  UnitLangs in 'Forms\UnitLangs.pas' {LangForm};

{$R *.res}

begin
  Application.Initialize;

  Application.MainFormOnTaskbar := True;
  Application.Title := 'TEBookConverter';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TFileInfoForm, FileInfoForm);
  Application.CreateForm(TLangForm, LangForm);
  Application.Run;

end.
