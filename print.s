	movq %rax, %rsi #
	movq $msg, %rdi #
	pushq %rax #
	pushq %rcx #
	movq $0, %rax #
	call printf #
	popq %rcx #
	popq %rax #
	
