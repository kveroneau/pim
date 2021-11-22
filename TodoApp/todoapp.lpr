program todoapp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, todolist, newtask
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Kevin''s Todo List';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TTodoForm, TodoForm);
  Application.Run;
end.

