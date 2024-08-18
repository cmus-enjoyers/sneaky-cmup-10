let cmus_music_path = "/home/vktrenokh/music"

let list_dir path = 
  Sys.readdir (path) |> Array.to_list

let is_not_playlist name = String.starts_with ~prefix:"." name |> Bool.not

let () = list_dir cmus_music_path |> List.filter(is_not_playlist) |> List.iter(print_endline)
