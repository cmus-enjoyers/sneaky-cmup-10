let cmus_music_path = "/home/vktrenokh/music"
let cmus_playlist_path = "/home/vktrenokh/.config/cmus/playlists"
let cmus_music_formats = ["mp3"; "opus"; "flac"]

type cmus_playlist = {
  name: string;
  path: string;
  content: string list;
}

let make_path path1 path2 = path1 ^ "/" ^ path2

let make_cmup_path = make_path cmus_music_path
let make_playlist_path = make_path cmus_playlist_path

let is_music name =
  cmus_music_formats |> List.exists(fun format -> String.ends_with ~suffix:format name)

let list_dir path = 
  Sys.readdir (path) |> Array.to_list

let write_list_to_file path_ list = 
  let oc = open_out path_ in
  list |> List.iter(fun str -> Printf.fprintf oc "%s\n" str);
  close_out oc

let write_playlist playlist = 
  print_endline playlist.name;
  write_list_to_file playlist.path playlist.content

let is_not_playlist name = String.starts_with ~prefix:"." name |> Bool.not

let create_cmus_playlist name = 
  let content = make_cmup_path name |> list_dir |> List.filter(is_music) in 
  { name = name; path = make_playlist_path name; content = content; }

let print_cmup_playlist playlist =
  print_endline ("Playlist " ^ playlist.name ^ " on path" ^ playlist.path);
  playlist.content |> List.iter(fun t -> print_endline("  " ^ t)) 

let () = 
  let playlists = list_dir cmus_music_path 
                  |> List.filter is_not_playlist 
                  |> List.map create_cmus_playlist
  in
  playlists |> List.iter write_playlist;
  playlists |> List.iter print_cmup_playlist
