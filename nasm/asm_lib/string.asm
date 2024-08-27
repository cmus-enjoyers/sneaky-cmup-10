%ifndef _STRING_
%define _STRING_

section .text

; int strlen(char *str)
strlen:
  push ebx
  mov ebx, eax

.nextchar:
  cmp byte [eax], 0
  jz .end
  inc eax
  jmp .nextchar

.end:
  sub eax, ebx   
  pop ebx
  ret

; char *get_end_of_string(char *src)
strend:
  push edx

  mov edx, eax
  call strlen
  add eax, edx

  pop edx
  ret
  

; char *strncpy(char *src, char *dest, int n)
strncpy:
  push edi
  push esi

  mov esi, eax
  mov edi, ebx

  rep movsb

  mov eax, ebx

  pop esi
  pop edi
  ret

; char *strcpy(char *src, char *dest)
strcpy:
  push edi
  push esi
  push ecx

  mov esi, eax
  call strlen
  mov ecx, eax
  mov eax, esi
  call strncpy

  pop ecx
  pop esi
  pop edi
  ret

; char *strcat(char *src, char *string)
strcat:
  push edx
  push ecx
  push ebx
  
  mov edx, eax
  mov ecx, ebx

  call strend
  mov ebx, eax
  mov eax, ecx
  call strcpy

  mov eax, edx
  pop ebx
  pop ecx
  pop edx
  ret

; char *strcut(char *src, int n)
strcut:
  push edi
  push edx
  mov edx, eax

  lea edi, [eax + ebx]
.nextchar:
  mov eax, 0
  stosb 
  cmp byte [edi], 0
  jnz .nextchar

  mov eax, edx

  pop edx
  pop edi
  ret

; int strfind(char *src, char symbol) ; ascii only
strfind:
  mov edx, eax

.nextchar:
  cmp byte [eax], bl
  je .end
  cmp byte [eax], 0
  je .end
  inc eax
  jmp .nextchar

.end:
  sub eax, edx
  ret

%endif ; _STRING_
