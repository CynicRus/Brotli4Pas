program brotli_demo;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  libbrotli in '..\..\src\libbrotli.pas',
  brotli in '..\..\src\brotli.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
