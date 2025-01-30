program Cmup;
{$mode objfpc}

uses SysUtils, StrUtils, Classes;

type
  TAudioFile = record
    FileName: string;
    TrackNumber: Integer;
  end;

function ExtractTrackNumberFromFileName(const FileNameWithExt: string): Integer;
var
  i, StartPos, EndPos: Integer;
  NumStr, FileName: String;
begin
  ExtractTrackNumberFromFileName := -1;

  if Length(FileNameWithExt) > 4 then
    if Copy(FileNameWithExt, Length(FileNameWithExt) - 3, 4) = '.mp3' then
      FileName := Copy(FileNameWithExt, 1, Length(FileNameWithExt) - 4)
    else
      FileName := FileNameWithExt
  else
    FileName := FileNameWithExt;

  StartPos := -1;

  for i := 1 to Length(FileName) do
  begin
    if FileName[i] = '#' then
    begin
      StartPos := i + 1;
      break;
    end
    else if (not (FileName[i] in ['0'..'9'])) and (StartPos <> -1) then
    begin
      EndPos := i - 1;
      NumStr := Copy(FileName, StartPos, EndPos - StartPos + 1);
      ExtractTrackNumberFromFileName := StrToIntDef(NumStr, -1);
      Exit;
    end;
  end;

  StartPos := -1;

  for i := 1 to Length(FileName) do
  begin
    if (FileName[i] in ['0'..'9']) and (StartPos = -1) then
      StartPos := i
    else if (FileName[i] = '.') and (StartPos <> -1) then
    begin
      EndPos := i - 1;
      NumStr := Copy(FileName, StartPos, EndPos - StartPos + 1);
      ExtractTrackNumberFromFileName := StrToIntDef(NumStr, -1);
      Exit;
    end;
  end;

  StartPos := -1;
  
  for i := Length(FileName) downto 1 do
  begin
    if FileName[i] in ['0'..'9'] then
    begin
      if StartPos = -1 then
        StartPos := i;
    end
    else
    begin
      if StartPos <> -1 then
      begin
        EndPos := i + 1;
        NumStr := Copy(FileName, EndPos, StartPos - EndPos + 1);
        ExtractTrackNumberFromFileName := StrToIntDef(NumStr, -1);
        Exit;
      end;
    end;
  end;
  
  if StartPos <> -1 then
  begin
    NumStr := Copy(FileName, 1, StartPos);
    ExtractTrackNumberFromFileName := StrToIntDef(NumStr, -1);
  end;
end;

procedure SortByTrackNumber(var Arr: Array of TAudioFile);
var
  i, j: Integer;
  Temp: TAudioFile;
begin
  for i := 0 to High(Arr) - 1 do
    for j := 0 to High(Arr) - 1 - i do
      if (Arr[j].TrackNumber = -1) or 
         (Arr[j].TrackNumber > Arr[j + 1].TrackNumber) then
      begin
        Temp := Arr[j];
        Arr[j] := Arr[j + 1];
        Arr[j + 1] := Temp;
      end;
end;

procedure ProcessAudioFiles(var AudioFiles: Array of TAudioFile; Files: TStringList);
var
  TrackNum: Integer;
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do
  begin
    TrackNum := ExtractTrackNumberFromFileName(Files[i]);

    AudioFiles[i].FileName := Files[i];
    AudioFiles[i].TrackNumber := TrackNum;
  end;

  SortByTrackNumber(AudioFiles);
end;

function GetNestedDirectoreies(const Directory: String): TStringList;
var
  SearchRec: TSearchRec;
  Directories: TStringList;
begin
  Directories := TStringList.Create;
  
  if FindFirst(Directory + '/*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = faDirectory then
          Directories.Add(SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
    
    FindClose(SearchRec);
  end;
  
  GetNestedDirectoreies := Directories;
end;

function GetFiles(const Directory: String): TStringList;
var
  SearchRec: TSearchRec;
  Files: TStringList;
begin
  Files := TStringList.Create;
  
  if FindFirst(Directory + '/*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) <> faDirectory then
          Files.Add(SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
    
    FindClose(SearchRec);
  end;
  
  GetFiles := Files;
end;

function Tree(const Directory: String): TStringList;
var
  Directories, InnerDirectories: TStringList;
  i: Integer;
begin
  Directories := GetNestedDirectoreies(Directory);
  Tree := TStringList.Create;
  InnerDirectories := TStringList.Create;

  if Directories.Count = 0 then
  begin
    Tree.Add(Directory);
    InnerDirectories.Free;
    Directories.Free;
    exit;
  end;

  for i := 0 to Directories.Count - 1 do
  begin
    InnerDirectories := Tree(Directory + '/' + Directories[i]);
    Tree.AddStrings(InnerDirectories);
    InnerDirectories.Free;
  end;

  Directories.Free;
end;

function CleanDirName(const DirName: String): String;
begin
  CleanDirName := ReplaceStr(DirName, '$', '');
end;

function PathToName(const Path: String): String;
var 
  Parts, RestParts: TStringArray;
  FirstPart, Rest, FirstOfRest: String;
  FirstPartLength: Integer;
begin
  Parts := SplitString(Path, '/');
  FirstPart := Parts[0];
  if Length(Parts) = 1 then
  begin
    PathToName := CleanDirName(FirstPart);
  end
  else
  begin
    FirstPartLength := Length(FirstPart);
    Rest := copy(Path, FirstPartLength + 2, Length(Path) - FirstPartLength);
    RestParts := SplitString(Rest, '/');
    FirstOfRest := RestParts[0];
    if FirstOfRest[Length(FirstOfRest)] = '$' then
      PathToName := CleanDirName(FirstPart + '-' + PathToName(Rest))
    else
      PathToName := CleanDirName(PathToName(Rest));
  end;
end;

function ConvertToPlaylistName(const DirectoryName: String; const BaseDirectoryLength: Integer): String;
var
  i, DirectoryLength: Integer;
  StrippedPath: String;
begin
  ConvertToPlaylistName := '';
  for i := 1 to Length(DirectoryName) do
  begin
    DirectoryLength := Length(DirectoryName);
    StrippedPath := copy(DirectoryName, BaseDirectoryLength + 2, DirectoryLength - BaseDirectoryLength);
    ConvertToPlaylistName := PathToName(StrippedPath);
  end;
end;

var
  i, j, BaseDirectoryLength: Integer;
  PathToMusicDirectory, PathToCmusPlaylists, PlaylistName: String;
  Directories, Files: TStringList;
  AudioFiles: Array of TAudioFile;
  CurrentFile: TextFile;
begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: ', ParamStr(0), ' <path_to_music_Directory> <path_to_cmus_playlists>');
    WriteLn('Example: ', ParamStr(0), ' ~/Music ~/.config/cmus/playlists');
    Halt(1);
  end;
  
  PathToMusicDirectory := ParamStr(1);
  PathToCmusPlaylists := ParamStr(2);

  Directories := Tree(PathToMusicDirectory);

  BaseDirectoryLength := Length(PathToMusicDirectory);

  for i := 0 to Directories.Count - 1 do
  begin
    PlaylistName := ConvertToPlaylistName(Directories[i], BaseDirectoryLength);
    Files := GetFiles(Directories[i]);
    SetLength(AudioFiles, Files.Count);
    ProcessAudioFiles(AudioFiles, Files);

    AssignFile(CurrentFile, PathToCmusPlaylists + '/' + PlaylistName);
    ReWrite(CurrentFile);

    for j := 0 to Files.Count - 1 do
    begin
      WriteLn(CurrentFile, Directories[i] + '/' + AudioFiles[j].FileName);
    end;

    CloseFile(CurrentFile);
  end;

  Directories.Free;
end.
