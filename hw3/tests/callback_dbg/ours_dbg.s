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
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$40, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	foo(%rip), %rdi
	leaq	ll_callback(%rip), %rax
	callq	*%rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	leaq	ll_ltoa(%rip), %rax
	callq	*%rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	leaq	ll_puts(%rip), %rax
	callq	*%rax
	movq	%rax, -40(%rbp)
	movq	$0, %rax
	addq	$40, %rsp
	popq	%rbp
	retq
