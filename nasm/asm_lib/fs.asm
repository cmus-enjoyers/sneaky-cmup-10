%ifndef _FS_
%define _FS_
%include "./asm_lib/common.asm"

section .bss
  stat_buffer resb 512

section .text

;void *nedomalloc(int size)
nedomalloc: 
  push edx
  push ecx
  push ebx

  mov edx, eax

  mov eax, 45
  xor ebx, ebx
  int 80h

  mov ecx, eax

  add eax, edx
  mov ebx, eax
  mov eax, 45
  int 80h

  mov eax, ecx

  pop ebx
  pop ecx
  pop edx
  ret

; int get_fd(path)
get_fd:
  push ecx
  push ebx 
  mov ebx, eax
  mov eax, 5
  mov ecx, 0
  int 80h
  pop ecx
  pop ebx
  ret

;NOTE: divide into smaller parts(later ¯\_(ツ)_/¯)

; void append(char *path, char *buff)
append:
  push esi
  push edx
  push ecx
  push ebx
  push eax

  mov esi, ebx

  mov ebx, eax
  mov eax, 5
  mov ecx, 0x441 ; O_WRONLY | O_APPEND | O_CREAT
  mov edx, 444
  int 80h

  mov ebx, eax
  mov ecx, esi 

  mov eax, esi
  call strlen
  mov edx, eax
  mov eax, 4
  int 80h

  mov eax, ebx
  call close_fd

  pop eax
  pop ebx
  pop ecx
  pop edx
  pop esi
  ret

;struct linux_dirent {
;     unsigned long  d_ino;     /* Inode number */
;     unsigned long  d_off;     /* Offset to next linux_dirent */
;     unsigned short d_reclen;  /* Length of this linux_dirent */
;     char           d_name[];  /* Filename (null-terminated) */
;   }

;linux_dirent *readdir(int fd)
readdir:
  push edx
  push ecx
  push ebx

  mov edx, 8191

  mov ebx, eax
  mov eax, edx
  call nedomalloc
  mov ecx, eax
  mov eax, 141
  int 80h

  mov eax, ecx

  pop ebx
  pop ecx
  pop edx
  ret

;int is_directocry(char *filename)
is_directory:
  push ecx 
  push ebx

  mov ebx,eax
  mov eax, 195
  mov ecx, stat_buffer
  int 80h

  mov eax, [ecx + 16]
  and eax, 0xf000
  cmp eax, 4000h
  jne .end
  mov eax, 1

.end:
  pop ebx
  pop ecx
  ret

; void close_fd(int fd)
close_fd:
  push eax
  push ebx

  mov ebx, eax
  mov eax, 6
  int 80h

  pop ebx
  pop eax
  ret

; NOTE: divide into smaller procedures (later ¯\_(ツ)_/¯)
; char *strslice(char *path)
get_dirname:
  push edi
  push esi
  push edx
  push ecx
  push ebx

  mov esi, eax
  xor ecx, ecx
  call strend

.find_slash:
  dec eax
  cmp byte [eax], "/"
  jne .find_slash
  cmp ecx, 0
  je .found

  lea edi, [eax+1]
  sub eax, esi
  sub edx, eax
  dec edx

  mov ecx, edx
  mov eax, ecx
  call nedomalloc
  mov ebx, eax
  mov eax, edi
  call strncpy

  pop ebx
  pop ecx
  pop edx
  pop esi
  pop edi
  ret

.found:
  mov edx, eax
  sub edx, esi
  inc ecx
  jmp .find_slash


%endif ; _FS_
