	.text
	.globl	foo
foo:
	leaq	(%rdi,%rsi), %rax
	retq

	.text
	.globl	main
main:
	pushq	%rax
	movq	$foo, %rdi
	callq	ll_callback
	movq	%rax, %rdi
	callq	ll_ltoa
	movq	%rax, %rdi
	callq	ll_puts
	movq	$0, %rax
	popq	%rcx
	retq
