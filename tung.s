	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $T0, %rdi
	pushq %rax
	movq $0, %rax
	call printf
	popq %rax
	leave
	ret
	.data
T0:
	.string "hello"
msg:
	.string "%d\n"
