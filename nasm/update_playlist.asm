%include "./asm_lib/common.asm"
%include "./asm_lib/fs.asm"
%include "./asm_lib/string.asm"

; NOTE:  divide into smaller procedures + no side effects pls 
; char *update_playlist(char *music_entry)
update_playlist:
  push ebp
  push edi
  push esi
  push edx
  push ecx
  push ebx

  cmp word [eax], "."
  je .end
  cmp word [eax], ".."
  je .end

  mov ebx, eax ;entry name save

  mov eax, path_buffer
  call strend
  mov edi, eax
  mov eax, "/"
  stosb 

  mov eax, path_buffer
  call strlen
  mov esi, eax 

  mov eax, path_buffer
  call strcat

  call is_directory
  cmp eax, 1
  jne .cmus

  mov eax, path_buffer
  call get_fd
  mov ebp, eax
  call readdir
  mov edx, eax

.foreach_entry:
  lea eax, [edx+10]
  call update_playlist
  movzx ecx, byte [edx + 8]
  add edx, ecx

  cmp byte [edx + 8], 0
  jne .foreach_entry

.close_dir:
  push eax
  mov eax, testString
  call printLF 
  pop eax

  push eax
  mov eax, ebp
  call close_fd
  pop eax
  jmp .subpath

; NOTE rewrite this bs
.cmus:
  push edi
  push ebx
  push eax

  mov eax, cmus_path_buffer
  mov ebx, cmus_playlist_path_length - 1
  call strcut

  mov eax, path_buffer
  call get_dirname
  mov ebx, eax
  mov eax, cmus_path_buffer
  call strcat

  mov eax, path_buffer
  call strend
  mov edi, eax
  mov eax, 0ah
  stosb
  
  mov ebx, path_buffer
  mov eax, cmus_path_buffer
  call append

  pop eax
  pop ebx
  pop edi
  jmp .subpath

.subpath:
  push eax
  mov eax, path_buffer
  mov ebx, esi
  dec ebx
  call strcut

  pop eax

.end: 
  pop ebx
  pop ecx
  pop edx
  pop esi
  pop edi
  pop ebp
  ret
