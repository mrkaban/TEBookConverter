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

unit UnitFileInfo;

interface

uses Classes, Windows, SysUtils, JvCreateProcess, Messages, StrUtils;

type
  TStatus = (fsReading, fsDone);

type
  TFileInfo = class(TObject)
  private
    FProcess: TJvCreateProcess;
    FStatus: TStatus;
    FFileName: string;
    FFFMpegPath: string;
    FInfoList: TStringList;

    procedure ProcessRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
    procedure ProcessTerminate(Sender: TObject; ExitCode: Cardinal);

  public
    property Status: TStatus read FStatus;
    property InfoList: TStringList read FInfoList;

    constructor Create(const FileName: string; const FFMpegPath: string);
    destructor Destroy(); override;

    procedure Start();
  end;

implementation

{ TFileInfo }

constructor TFileInfo.Create(const FileName: string; const FFMpegPath: string);
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

  FStatus := fsReading;
  FFileName := FileName;
  FFFMpegPath := FFMpegPath;
  FInfoList := TStringList.Create;

end;

destructor TFileInfo.Destroy;
begin
  inherited Destroy;
  FProcess.Free;
  FreeAndNil(FInfoList);
end;

procedure TFileInfo.ProcessRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
begin

end;

procedure TFileInfo.ProcessTerminate(Sender: TObject; ExitCode: Cardinal);
begin

  FStatus := fsReading;
  try
    FInfoList.AddStrings(FProcess.ConsoleOutput);
  finally
    FStatus := fsDone;
  end;

end;

procedure TFileInfo.Start;
begin

  FProcess.ApplicationName := FFFMpegPath;
  FProcess.CommandLine := ' "' + FFileName + '"';
  FProcess.Run;

end;

end.
