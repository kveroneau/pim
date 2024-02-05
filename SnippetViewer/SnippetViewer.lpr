program SnippetViewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, SnippetWindow, PushPullWindow, ViewerWindow
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Snippet Viewer';
  Application.Scaled:=True;
  Application.ExceptionDialog:=aedOkMessageBox;
  Application.Initialize;
  Application.CreateForm(TSnippetForm, SnippetForm);
  Application.CreateForm(TPushPullForm, PushPullForm);
  Application.Run;
end.

