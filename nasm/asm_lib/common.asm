%ifndef _COMMON_
%define _COMMON_

%include "./asm_lib/string.asm"

; void print(char *message)
print:
  push edx
  push ecx
  push ebx
  push eax

  mov ecx, eax
  call strlen

  mov edx, eax
  mov eax, 4
  mov ebx, 1
  int 80h

  pop eax
  pop ebx
  pop ecx
  pop edx
  ret

; void print(char *message)
printLF:
  push eax
  call print
  mov eax, 0ah
  push eax
  mov eax, esp
  call print
  pop eax
  pop eax
  ret

; void quit()
quit: 
  mov eax, 1
  xor ebx, ebx
  int 80h
  ret

%endif ; _COMMON_
