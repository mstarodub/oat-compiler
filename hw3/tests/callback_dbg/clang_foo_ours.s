	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$56, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	-56(%rbp), %r10
	movq	%r10, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	addq	%rcx, %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	addq	$56, %rsp
	popq	%rbp
	retq
	.text
	.globl	main
main:
	pushq	%rax
	movl	$foo, %edi
	callq	ll_callback
	movq	%rax, %rdi
	callq	ll_ltoa
	movq	%rax, %rdi
	callq	ll_puts
	xorl	%eax, %eax
	popq	%rcx
	retq
