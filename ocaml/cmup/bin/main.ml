let cmus_music_path = "/home/vktrenokh/music"
let cmus_music_formats = ["mp3"; "opus"; "flac"]

type cmus_playlist = {
  name: string;
  path: string;
  content: string list;
}

let make_cmup_path name = cmus_music_path ^ "/" ^ name

let is_music name =
  cmus_music_formats |> List.exists(fun format -> String.ends_with ~suffix:format name)

let list_dir path = 
  Sys.readdir (path) |> Array.to_list

let is_not_playlist name = String.starts_with ~prefix:"." name |> Bool.not

let create_cmus_playlist name = 
  let path = make_cmup_path name in
  let content = list_dir path |> List.filter(is_music) in 
  { name = name; path = path; content = content; }

let print_cmup_playlist playlist =
  print_endline ("Playlist " ^ playlist.name ^ " on path" ^ playlist.path);
  playlist.content |> List.iter(fun t -> print_endline("  " ^ t)) 

let () = list_dir cmus_music_path
                  |> List.filter(is_not_playlist)
                  |> List.map(create_cmus_playlist) 
                  |> List.iter(print_cmup_playlist)
