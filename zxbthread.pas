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
  Buffer: array[0..127] of char;
  ReadCount: Integer;
  ReadSize: Integer;
begin
  FZXBProcess.Executable := FExecutable;

  FZXBProcess.Options := [poUsePipes, poStdErrToOutPut];
  FZXBProcess.ShowWindow := swoHide;
  FZXBProcess.Execute;

  while FZXBProcess.Running do
  begin
    if FZXBProcess.Output.NumBytesAvailable > 0 then
    begin
      ReadSize := FZXBProcess.Output.NumBytesAvailable;
      if ReadSize > SizeOf(Buffer) then ReadSize := SizeOf(Buffer);
      ReadCount := FZXBProcess.Output.Read(Buffer[0], ReadSize);
      FOutput := Copy(Buffer, 0, ReadCount);
      Synchronize(@DataAvailable);
    end;
  end;

  FZXBProcess.Free;
end;

end.

