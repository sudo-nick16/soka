.intel_syntax noprefix

num_len = 21

msg: .string "hello world\n"
msg_len = . - msg

.global _start
print_num:
	# rdi has num
	sub rsp, num_len

	xor r9, r9
	mov r9, num_len
	dec r9

	mov byte ptr [rsp+r9], 0
	dec r9

	L0:
	cmp rdi, 0
	jle L1

	xor rax, rax
	mov rax, rdi
	mov rcx, 10
	cqo
	div rcx

	xor rdi, rdi
	mov rdi, rax

	add edx, 48
	mov byte [rsp+r9], dl
	dec r9

	jmp L0

	L1:

	mov rax, 1
	mov rdi, 1
	lea rsi, [rsp+r9]
	mov r8, num_len
	sub r8, r9
	mov rdx, r8
	syscall

	add rsp, num_len
	ret

_start:
	pushq 20
	pushq 59
	pop rax
	pop rbx
	add rax, rbx
	push rax


	pop rdi
	call print_num

	mov rax, 1
	mov rdi, 1
	lea rsi, msg
	mov rdx, msg_len
	syscall


	mov rax,  60
	mov rdi, 0
	syscall

