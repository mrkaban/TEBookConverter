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

unit UnitInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, sSkinProvider, Vcl.ComCtrls, sListView, UnitFileInfo;

type
  TFileInfoForm = class(TForm)
    InfoList: TsListView;
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FMetaPath: string;
  public
    { Public declarations }
    FileName: string;
  end;

var
  FileInfoForm: TFileInfoForm;

implementation

{$R *.dfm}

uses UnitMain;

procedure TFileInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.Enabled := True;
  MainForm.BringToFront;
end;

procedure TFileInfoForm.FormCreate(Sender: TObject);
begin
  FMetaPath := ExtractFileDir(Application.ExeName) + '\Calibre\ebook-meta.exe';
end;

procedure TFileInfoForm.FormResize(Sender: TObject);
begin
  InfoList.Columns[0].Width := 150;
  InfoList.Columns[1].Width := InfoList.ClientWidth - InfoList.Columns[0].Width;
end;

procedure TFileInfoForm.FormShow(Sender: TObject);
var
  LFI: TFileInfo;
  I: Integer;
  LLine: string;
  LKey, LValue: string;
  LItem: TListItem;
  LPos: integer;
begin
  if FileExists(FileName) then
  begin
    LFI := TFileInfo.Create(FileName, FMetaPath);
    InfoList.Items.Clear;
    try
      LFI.Start;
      while LFI.Status = fsReading do
      begin
        Application.ProcessMessages;
        Sleep(50);
      end;
      if LFI.InfoList.Count > 0 then
      begin
        for I := 0 to LFI.InfoList.Count-1 do
        begin
          LLine := trim(LFI.InfoList[i]);
          if Length(LLine) > 0 then
          begin
            LPos := Pos(':', LLine);
            if LPos > 1 then
            begin
              LKey := Copy(LLine, 1, LPos-1);
              LValue := Copy(LLine, LPos+1, MaxInt);
              LItem := InfoList.Items.Add;
              LItem.Caption := Trim(LKey);
              LItem.SubItems.Add(Trim(LValue));
            end;
          end;
        end;
      end;
    finally
      LFI.Free;
    end;
  end;
end;

end.
