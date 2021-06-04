unit zxbthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

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

procedure SaveDebug(s: String);
function GetZXBASICVersion(Path: String): String;

implementation

procedure SaveDebug(s: String);
var
  fo: TStrings;
  d: String;
begin
  fo := TStringList.Create;
  if FileExists('C:\tmp\debug.txt') then fo.LoadFromFile('C:\tmp\debug.txt');
  DateTimeToString(d,'hh:nn:ss.zzz',Now);
  fo.Add('['+d+'] '+s);
  fo.SaveToFile('C:\tmp\debug.txt');
  fo.Free;
end;

function GetZXBASICVersion(Path: String): String;
var
  Buffer: array[0..127] of char;
  ReadCount: Integer;
  ReadSize: Integer;
  p: TProcess;
  s: String;
begin
  p := TProcess.Create(nil);
  p.Executable := Path;
  p.Parameters.Add('--version');
  p.Options := [poUsePipes, poStdErrToOutPut];
  p.ShowWindow := swoHide;
  p.Execute;
  s := '';
  while p.Running do
  begin
    if p.Output.NumBytesAvailable > 0 then
    begin
      ReadSize := p.Output.NumBytesAvailable;
      if ReadSize > SizeOf(Buffer) then ReadSize := SizeOf(Buffer);
      ReadCount := p.Output.Read(Buffer[0], ReadSize);
      s := s + Copy(Buffer, 0, ReadCount);
    end;
  end;
  Result := s;
  p.Free;
end;

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
  SaveDebug('ZXB start');
  while FZXBProcess.Running do
  begin
    if FZXBProcess.Output.NumBytesAvailable > 0 then
    begin
      ReadSize := FZXBProcess.Output.NumBytesAvailable;
      SaveDebug('Data available: ' + ReadSize.ToString + ' bytes');
      if ReadSize > SizeOf(Buffer) then ReadSize := SizeOf(Buffer);
      ReadCount := FZXBProcess.Output.Read(Buffer[0], ReadSize);
      FOutput := Copy(Buffer, 0, ReadCount);
      Synchronize(@DataAvailable);
    end;
  end;
  SaveDebug('ZXB finish');
  FZXBProcess.Free;
end;

end.

