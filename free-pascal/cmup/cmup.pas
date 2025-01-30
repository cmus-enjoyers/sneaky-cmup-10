program Cmup;
{$mode objfpc}

uses SysUtils, StrUtils, Classes, Process, Crt;

type
  TIntegerArray = array of Integer;
  TAudioFile = record
    FileName: string;
    TrackNumber: Integer;
  end;

function GetMetadataTrackNumbers(const FilePaths: TStringList): TIntegerArray;
var
  AProcess: TProcess;
  OutputLines: TStringList;
  i: Integer;
  TrackStr: String;
begin
  GetMetadataTrackNumbers := nil;
  SetLength(GetMetadataTrackNumbers, FilePaths.Count);
  for i := 0 to High(GetMetadataTrackNumbers) do
    GetMetadataTrackNumbers[i] := -1;

  if FilePaths.Count = 0 then
    Exit;

  OutputLines := TStringList.Create;
  try
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'ffprobe';
      AProcess.Parameters.Add('-v');
      AProcess.Parameters.Add('error');
      AProcess.Parameters.Add('-select_streams');
      AProcess.Parameters.Add('a:0');
      AProcess.Parameters.Add('-show_entries');
      AProcess.Parameters.Add('format_tags=track');
      AProcess.Parameters.Add('-of');
      AProcess.Parameters.Add('default=noprint_wrappers=1:nokey=1');
      for i := 0 to FilePaths.Count - 1 do
        AProcess.Parameters.Add(FilePaths[i]);

      AProcess.Options := [poUsePipes, poWaitOnExit];
      AProcess.Execute;
      AProcess.WaitOnExit;

      OutputLines.LoadFromStream(AProcess.Output);

      for i := 0 to OutputLines.Count - 1 do
      begin
        if i >= FilePaths.Count then
          Break; // Avoid index out of bounds
        TrackStr := Trim(OutputLines[i]);
        if TrackStr <> '' then
          GetMetadataTrackNumbers[i] := StrToIntDef(TrackStr, -1);
      end;
    finally
      AProcess.Free;
    end;
  finally
    OutputLines.Free;
  end;
end;
  
function GetMetadataTrackNumber(const FilePath: String): Integer;
var
  AProcess: TProcess;
  OutputLines: TStringList;
  TrackStr: String;
begin
  GetMetadataTrackNumber := -1;

  if not FileExists(FilePath) then
    Exit;

  OutputLines := TStringList.Create;
  try
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'ffprobe';
      AProcess.Parameters.Add('-v');
      AProcess.Parameters.Add('error');
      AProcess.Parameters.Add('-select_streams');
      AProcess.Parameters.Add('a:0');
      AProcess.Parameters.Add('-show_entries');
      AProcess.Parameters.Add('format_tags=track');
      AProcess.Parameters.Add('-of');
      AProcess.Parameters.Add('default=noprint_wrappers=1:nokey=1');
      AProcess.Parameters.Add(FilePath);
      AProcess.Options := [poUsePipes];

      AProcess.Execute;
      AProcess.WaitOnExit;

      OutputLines.LoadFromStream(AProcess.Output);
      if OutputLines.Count > 0 then
      begin
        TrackStr := Trim(OutputLines[0]);
        if TrackStr <> '' then
          GetMetadataTrackNumber := StrToIntDef(TrackStr, -1);
      end;
    finally
      AProcess.Free;
    end;
  finally
    OutputLines.Free;
  end;
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

procedure QuickSort(var Arr: array of TAudioFile; Left, Right: Integer);
var
  I, J: Integer;
  Pivot, Temp: TAudioFile;
begin
  if Left >= Right then Exit;
  
  I := Left;
  J := Right;
  Pivot := Arr[(Left + Right) div 2];
  
  repeat
    while (Arr[I].TrackNumber < Pivot.TrackNumber) or 
          ((Arr[I].TrackNumber = Pivot.TrackNumber) and (I < (Left + Right) div 2)) do
      Inc(I);
      
    while (Arr[J].TrackNumber > Pivot.TrackNumber) or 
          ((Arr[J].TrackNumber = Pivot.TrackNumber) and (J > (Left + Right) div 2)) do
      Dec(J);
      
    if I <= J then
    begin
      Temp := Arr[I];
      Arr[I] := Arr[J];
      Arr[J] := Temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  
  if Left < J then QuickSort(Arr, Left, J);
  if I < Right then QuickSort(Arr, I, Right);
end;

procedure SortByTrackNumber(var Arr: Array of TAudioFile);
begin
  if Length(Arr) > 1 then
    QuickSort(Arr, 0, High(Arr));
end;

procedure ProcessAudioFiles(var AudioFiles: Array of TAudioFile; Files: TStringList; const BaseDirectory: String);
var
  TrackNums: TIntegerArray;
  FilePaths: TStringList;
  i: Integer;
begin
  if Files.Count = 0 then
    Exit;

  FilePaths := TStringList.Create;
  try
    for i := 0 to Files.Count - 1 do
      FilePaths.Add(BaseDirectory + '/' + Files[i]);

    TrackNums := GetMetadataTrackNumbers(FilePaths);

    for i := 0 to Files.Count - 1 do
    begin
      AudioFiles[i].FileName := Files[i];
      if TrackNums[i] <> -1 then
        AudioFiles[i].TrackNumber := TrackNums[i]
      else
        AudioFiles[i].TrackNumber := ExtractTrackNumberFromFileName(Files[i]);
    end;
  finally
    FilePaths.Free;
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
  Queue: TStringList;
  CurrentDir: String;
  Directories: TStringList;
  i: Integer;
begin
  Tree := TStringList.Create;
  Queue := TStringList.Create;
  try
    Queue.Add(Directory);
    while Queue.Count > 0 do
    begin
      CurrentDir := Queue[0];
      Queue.Delete(0);
      Tree.Add(CurrentDir);
      
      Directories := GetNestedDirectoreies(CurrentDir);
      try
        for i := 0 to Directories.Count - 1 do
          Queue.Add(CurrentDir + '/' + Directories[i]);
      finally
        Directories.Free;
      end;
    end;
  finally
    Queue.Free;
  end;
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
    Rest := Copy(Path, FirstPartLength + 2, Length(Path) - FirstPartLength);
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
  StrippedPath: String;
begin
  StrippedPath := Copy(DirectoryName, BaseDirectoryLength + 2, MaxInt);
  Result := PathToName(StrippedPath);
end;

var
  i, j, BaseDirectoryLength: Integer;
  PathToMusicDirectory, PathToCmusPlaylists, PlaylistName: String;
  Directories, Files, PlaylistLines: TStringList;
  AudioFiles: Array of TAudioFile;
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

  for i := 1 to Directories.Count - 1 do
  begin
    WriteLn('Processing directory [', i + 1, '/', Directories.Count, '] ', Directories[i]);

    PlaylistName := ConvertToPlaylistName(Directories[i], BaseDirectoryLength);
    Files := GetFiles(Directories[i]);
    SetLength(AudioFiles, Files.Count);
    ProcessAudioFiles(AudioFiles, Files, Directories[i]);

    PlaylistLines := TStringList.Create;
    try
      for j := 0 to Files.Count - 1 do
        PlaylistLines.Add(Directories[i] + '/' + AudioFiles[j].FileName);
      PlaylistLines.SaveToFile(PathToCmusPlaylists + '/' + PlaylistName);
    finally
      PlaylistLines.Free;
    end;

    GotoXY(1, WhereY - 1);
    ClrEol;
  end;

  Directories.Free;
end.
