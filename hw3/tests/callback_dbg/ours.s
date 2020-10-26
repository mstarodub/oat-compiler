	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$56, %rsp
	movq	%rdi, %r10
	movq	%r10, -8(%rbp)
	movq	%rsi, %r10
	movq	%r10, -16(%rbp)
	leaq	-56(%rbp), %r10
	movq	%r10, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %r10
	movq	%r10, %rax
	movq	-16(%rbp), %r10
	movq	%r10, %rcx
	addq	%rcx, %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %r10
	movq	%r10, %rax
	movq	-24(%rbp), %r10
	movq	%r10, %rcx
	movq	%rax, (%rcx)
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %r10
	movq	%r10, %rax
	movq	(%rax), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %r10
	movq	%r10, %rax
	addq	$56, %rsp
	popq	%rbp
	retq	
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$40, %rsp
	movq	%rdi, %r10
	movq	%r10, -8(%rbp)
	movq	%rsi, %r10
	movq	%r10, -16(%rbp)
	subq	$0, %rsp
	leaq	foo(%rip), %r10
	movq	%r10, %rdi
	leaq	ll_callback(%rip), %rax
	callq	*%rax
	addq	$0, %rsp
	movq	%rax, -24(%rbp)
	subq	$0, %rsp
	movq	-24(%rbp), %r10
	movq	%r10, %r10
	movq	%r10, %rdi
	leaq	ll_ltoa(%rip), %rax
	callq	*%rax
	addq	$0, %rsp
	movq	%rax, -32(%rbp)
	subq	$0, %rsp
	movq	-32(%rbp), %r10
	movq	%r10, %r10
	movq	%r10, %rdi
	leaq	ll_puts(%rip), %rax
	callq	*%rax
	addq	$0, %rsp
	movq	%rax, -40(%rbp)
	movq	$0, %rax
	addq	$40, %rsp
	popq	%rbp
	retq	