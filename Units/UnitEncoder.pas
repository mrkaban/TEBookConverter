{ *
  * Copyright (C) 2014-2015 ozok <ozok26@gmail.com>
  *
  * This file is part of TEBookConverter.
  *
  * TEBookConverter is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2 of the License, or
  * (at your option) any later version.
  *
  * TEBookConverter is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with TEBookConverter.  If not, see <http://www.gnu.org/licenses/>.
  *
  * }
unit UnitEncoder;

interface

uses Classes, Windows, SysUtils, JvCreateProcess, Messages, StrUtils, Generics.Collections, ComCtrls;

// current state of the process
type
  TEncoderStatus = (esEncoding, esStopped, esDone);

type
  TEncoder = class(TObject)
  private
    // process
    FProcess: TJvCreateProcess;
    // list of command lines to be executed
    FCommandLines: TStringList;
    // list of executables
    FPaths: TStringList;
    // index of current command line. Also progress.
    FCommandIndex: integer;
    // last line backend has written to console
    FConsoleOutput: string;
    // encoder's state
    FEncoderStatus: TEncoderStatus;
    // flag to indicate if encoding is stopped by user
    FStoppedByUser: Boolean;
    // list of files to be processed.
    FOutputFolderName: TStringList;
    // a list of indexes indicating to the files in a list. to show progress in the list.
    FFileIndexes: TList<integer>;
    // a list of output files. generally to check if they are created.
    FOutputFiles: TStringList;
    FID: integer;
    FItem: TListItem;

    // process events
    procedure ProcessRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
    procedure ProcessTerminate(Sender: TObject; ExitCode: Cardinal);

    // field variable read funcs
    function GetProcessID: integer;
    function GetFileName: string;
    function GetCommandCount: integer;
    function GetFileIndex: Integer;
  public
    property ConsoleOutput: string read FConsoleOutput;
    property EncoderStatus: TEncoderStatus read FEncoderStatus;
    property CommandLines: TStringList read FCommandLines write FCommandLines;
    property EncoderPaths: TStringList read FPaths write FPaths;
    property OutputFolder: TStringList read FOutputFolderName;
    property FilesDone: integer read FCommandIndex;
    property ProcessID: integer read GetProcessID;
    property CurrentFile: string read GetFileName;
    property CommandCount: integer read GetCommandCount;
    property FileIndexes: TList<integer> read FFileIndexes write FFileIndexes;
    property FileIndex: Integer read GetFileIndex;
    property OutputFiles: TStringList read FOutputFiles write FOutputFiles;
    property ID: integer read FID write FID;

    constructor Create();
    destructor Destroy(); override;

    procedure Start();
    procedure Stop();
    procedure ResetValues();
    function GetConsoleOutput(): TStrings;
  end;

implementation

{ TEncoder }

uses UnitMain, UnitLogs;

constructor TEncoder.Create;
begin
  inherited Create;

  FProcess := TJvCreateProcess.Create(nil);
  with FProcess do
  begin
    OnRead := ProcessRead;
    OnTerminate := ProcessTerminate;
    ConsoleOptions := [coRedirect];
    CreationFlags := [cfUnicode];
    Priority := ppIdle;

    with StartupInfo do
    begin
      DefaultPosition := False;
      DefaultSize := False;
      DefaultWindowState := False;
      ShowWindow := swHide;
    end;

    WaitForTerminate := true;
  end;

  FCommandLines := TStringList.Create;
  FPaths := TStringList.Create;
  FOutputFolderName := TStringList.Create;
  FEncoderStatus := esStopped;
  FStoppedByUser := False;
  FFileIndexes := TList<integer>.Create;
  FCommandIndex := 0;
  FOutputFiles := TStringList.Create;
end;

destructor TEncoder.Destroy;
begin

  inherited Destroy;
  FreeAndNil(FCommandLines);
  FreeAndNil(FPaths);
  FreeAndNil(FOutputFolderName);
  FreeAndNil(FFileIndexes);
  FreeAndNil(FOutputFiles);
  FProcess.Free;

end;

function TEncoder.GetCommandCount: integer;
begin
  Result := FCommandLines.Count;
end;

function TEncoder.GetConsoleOutput: TStrings;
begin
  Result := FProcess.ConsoleOutput;
end;

function TEncoder.GetFileIndex: Integer;
begin
  Result := -1;
  if FCommandIndex < FFileIndexes.Count then
    Result := FFileIndexes[FCommandIndex];
end;

function TEncoder.GetFileName: string;
begin
  if FCommandIndex < FOutputFolderName.Count then
    Result := FOutputFolderName[FCommandIndex];
end;

function TEncoder.GetProcessID: integer;
begin
  Result := FProcess.ProcessInfo.hProcess;
end;

procedure TEncoder.ProcessRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
begin
  FConsoleOutput := Trim(S);
end;

procedure TEncoder.ProcessTerminate(Sender: TObject; ExitCode: Cardinal);
begin
  if ExitCode <> 0 then
  begin
    FItem.SubItems[0] := 'Error';
    FItem.StateIndex := 2;
    MainForm.SaveToLog(FID, FProcess.ConsoleOutput, GetFileName);
  end
  else
  begin
    FItem.SubItems[0] := 'Done';
    FItem.StateIndex := 1;
  end;
  FProcess.ConsoleOutput.Clear;

  FEncoderStatus := esStopped;
  if FStoppedByUser then
  begin
    FEncoderStatus := esStopped;
  end
  else
  begin
    MainForm.UpdateProgress;

    // run next command
    inc(FCommandIndex);
    if FCommandIndex < FCommandLines.Count then
    begin
      FProcess.CommandLine := FCommandLines[FCommandIndex];
      FProcess.ApplicationName := FPaths[FCommandIndex];
      FEncoderStatus := esEncoding;
      FConsoleOutput := '';
      FItem := MainForm.ProgressList.Items.Add;
      FItem.Caption := GetFileName;
      FItem.SubItems.Add('Converting');
      FItem.MakeVisible(False);
      FItem.StateIndex := 0;
      if not DirectoryExists(ExtractFileDir(GetFileName)) then
      begin
        try
          ForceDirectories(ExtractFileDir(GetFileName))
        except
          on E: EInOutError do
          begin
            MainForm.AddToLog(0, Format(RS_6, [ExtractFileDir(GetFileName)]));
          end;
        end;
      end;
      FProcess.Run;
    end
    else
    begin
      // done
      FOutputFolderName.Clear;
      FEncoderStatus := esDone;
    end;
  end;
end;

procedure TEncoder.ResetValues;
begin
  // reset all lists, indexes etc
  FCommandLines.Clear;
  FPaths.Clear;
  FCommandIndex := 0;
  FConsoleOutput := '';
  FProcess.ConsoleOutput.Clear;
  FStoppedByUser := False;
end;

procedure TEncoder.Start;
begin
  if FProcess.ProcessInfo.hProcess = 0 then
  begin
    if FCommandLines.Count > 0 then
    begin
      if FileExists(FPaths[0]) then
      begin
        FProcess.ApplicationName := FPaths[0];
        FProcess.CommandLine := FCommandLines[0];
        FEncoderStatus := esEncoding;
        FItem := MainForm.ProgressList.Items.Add;
        FItem.Caption := GetFileName;
        FItem.SubItems.Add('Converting');
        FItem.MakeVisible(False);
        FItem.StateIndex := 0;
        if not DirectoryExists(ExtractFileDir(GetFileName)) then
        begin
          try
            ForceDirectories(ExtractFileDir(GetFileName))
          except
            on E: EInOutError do
            begin
              MainForm.AddToLog(0, Format(RS_6, [ExtractFileDir(GetFileName)]));
            end;
          end;
        end;
        FProcess.Run;
      end
      else
        FConsoleOutput := 'encoder'
    end
    else
      FConsoleOutput := '0 cmd'
  end
  else
    FConsoleOutput := 'not 0'
end;

procedure TEncoder.Stop;
begin
  if FProcess.ProcessInfo.hProcess > 0 then
  begin
    TerminateProcess(FProcess.ProcessInfo.hProcess, 0);
    FOutputFolderName.Clear;
    FEncoderStatus := esStopped;
    FStoppedByUser := true;
  end;
end;

end.
