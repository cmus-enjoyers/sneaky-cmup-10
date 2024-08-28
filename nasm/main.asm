%include "./asm_lib/common.asm"
%include "./update_playlist.asm"

section .data
  music_dir db "home/slikedollar/music",0h
  music_dir_length equ $ - music_dir
  cmus_playlist_path db "/home/slikedollar/.config/cmus/playlists/",0h
  cmus_playlist_path_length equ $ - cmus_playlist_path

section .bss
  path_buffer resb music_dir_length + 1000
  cmus_path_buffer resb cmus_playlist_path_length + 255
  dirname_buffer resb 255

section .text
global _start

_start:
  mov eax, cmus_playlist_path
  mov ebx, cmus_path_buffer 
  mov ecx, cmus_playlist_path_length
  call strncpy

  mov eax, music_dir
  ;mov ebx, testString
  ;call append

  ;call get_dirname
  ;call printLF

  ;mov ebx, "/"
  ;call strfind

  ;call strfind
  ;
  ;mov ebx, testString
  ;call strcat
  ;
  ;call printLF
  ;
  ;mov eax, path_buffer
  ;mov ebx, music_dir_length - 1
  ;call strcut
  ;
  ;call printLF
  ;
  ;lea eax, [path_buffer + 28]
  ;call printLF

  call update_playlist
  call quit
