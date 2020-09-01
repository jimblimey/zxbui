unit zxbthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Math;

type
  TOutputAvailableEvent = procedure(output: String) of Object;
  TZXBThread = class(TThread)
    private
      FOnOutputAvailable: TOutputAvailableEvent;
      FZXBProcess: TProcess;
      FExecutable: String;
      FOutput: String;
      procedure DataAvailable;
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: boolean);
      procedure SetParams(params: TStrings);
      property OnOutputAvailable: TOutputAvailableEvent read FOnOutputAvailable write FOnOutputAvailable;
      property Executable: String read FExecutable write FExecutable;
      property Output: String read FOutput write FOutput;
  end;

implementation

constructor TZXBThread.Create(CreateSuspended: Boolean);
begin
  FZXBProcess := TProcess.Create(nil);
  inherited Create(CreateSuspended);
end;

procedure TZXBThread.DataAvailable;
begin
  if Assigned(FOnOutputAvailable) then
  begin
    FOnOutputAvailable(FOutput);
  end;
end;

procedure TZXBThread.SetParams(params: TStrings);
begin
  FZXBProcess.Parameters.Clear;
  FZXBProcess.Parameters.AddStrings(params);
end;

procedure TZXBThread.Execute;
var
  CharBuffer: array [0..511] of char;
  ReadCount: integer;
begin
  FZXBProcess.Executable := FExecutable;

  FZXBProcess.Options := [poUsePipes, poStdErrToOutPut];
  FZXBProcess.ShowWindow := swoHide;
  FZXBProcess.Execute;

  while FZXBProcess.Running do
  begin
    while FZXBProcess.Output.NumBytesAvailable > 0 do
    begin
      ReadCount := Min(512, FZXBProcess.Output.NumBytesAvailable);
      FZXBProcess.Output.Read(CharBuffer, ReadCount);
      FOutput := Copy(CharBuffer, 0, ReadCount);
      Synchronize(@DataAvailable);
    end;
  end;

  FZXBProcess.Free;
end;

end.

