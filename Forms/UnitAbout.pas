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

unit UnitAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, sSkinProvider, Vcl.StdCtrls, sButton,
  sLabel, Vcl.ExtCtrls, acImage, acPNG, ShellAPI, Vcl.ImgList, acAlphaImageList,
  Vcl.ComCtrls, sListView, System.ImageList;

type
  TAboutForm = class(TForm)
    sImage1: TsImage;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sLabel3: TsLabel;
    sButton1: TsButton;
    sButton2: TsButton;
    sListView1: TsListView;
    LagImgs: TsAlphaImageList;
    procedure sButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

uses UnitMain;

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.Enabled := True;
  MainForm.BringToFront;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
{$IFDEF WIN32}
  sLabel1.Caption := sLabel1.Caption + ' 32bit';
{$ELSE}
  sLabel1.Caption := sLabel1.Caption + ' 64bit';
{$ENDIF}
end;

procedure TAboutForm.sButton1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'https://sourceforge.net/projects/tebookconverter/', nil, nil, SW_NORMAL);
end;

procedure TAboutForm.sButton2Click(Sender: TObject);
begin
  Self.Close;
end;

end.
