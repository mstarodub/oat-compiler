	.text
	.file	"cinterop_dbg.c"
	.globl	ll_puts                 # -- Begin function ll_puts
	.p2align	4, 0x90
	.type	ll_puts,@function
ll_puts:                                # @ll_puts
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	puts
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	ll_puts, .Lfunc_end0-ll_puts
	.cfi_endproc
                                        # -- End function
	.globl	ll_strcat               # -- Begin function ll_strcat
	.p2align	4, 0x90
	.type	ll_strcat,@function
ll_strcat:                              # @ll_strcat
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	callq	strlen
                                        # kill: def $eax killed $eax killed $rax
	movl	%eax, -20(%rbp)
	movq	-16(%rbp), %rdi
	callq	strlen
                                        # kill: def $eax killed $eax killed $rax
	movl	%eax, -24(%rbp)
	movl	-20(%rbp), %eax
	addl	-24(%rbp), %eax
	addl	$1, %eax
	movslq	%eax, %rdi
	movl	$1, %esi
	callq	calloc
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	movq	-8(%rbp), %rsi
	movslq	-20(%rbp), %rdx
	callq	strncpy
	movq	-32(%rbp), %rcx
	movslq	-20(%rbp), %rdx
	addq	%rdx, %rcx
	movq	-16(%rbp), %rsi
	movl	-24(%rbp), %r8d
	addl	$1, %r8d
	movslq	%r8d, %rdx
	movq	%rcx, %rdi
	movq	%rax, -40(%rbp)         # 8-byte Spill
	callq	strncpy
	movq	-32(%rbp), %rcx
	movq	%rax, -48(%rbp)         # 8-byte Spill
	movq	%rcx, %rax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end1:
	.size	ll_strcat, .Lfunc_end1-ll_strcat
	.cfi_endproc
                                        # -- End function
	.globl	ll_callback             # -- Begin function ll_callback
	.p2align	4, 0x90
	.type	ll_callback,@function
ll_callback:                            # @ll_callback
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movabsq	$.L.str, %rdi
	callq	puts
	movq	$19, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	-16(%rbp), %rdi
	movq	-16(%rbp), %rsi
	movl	%eax, -28(%rbp)         # 4-byte Spill
	callq	*%rcx
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rsi
	movabsq	$.L.str.1, %rdi
	movb	$0, %al
	callq	printf
	movq	-24(%rbp), %rcx
	movl	%eax, -32(%rbp)         # 4-byte Spill
	movq	%rcx, %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end2:
	.size	ll_callback, .Lfunc_end2-ll_callback
	.cfi_endproc
                                        # -- End function
	.globl	ll_ltoa                 # -- Begin function ll_ltoa
	.p2align	4, 0x90
	.type	ll_ltoa,@function
ll_ltoa:                                # @ll_ltoa
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movabsq	$.L.str.2, %rdi
	callq	puts
	movl	$20, %edi
	movl	$1, %esi
	movl	%eax, -20(%rbp)         # 4-byte Spill
	callq	calloc
	movq	%rax, -16(%rbp)
	movabsq	$.L.str.3, %rdi
	callq	puts
	movq	-16(%rbp), %rdi
	movq	-8(%rbp), %rcx
	movl	$20, %esi
	movabsq	$.L.str.4, %rdx
	movl	%eax, -24(%rbp)         # 4-byte Spill
	movb	$0, %al
	callq	snprintf
	movabsq	$.L.str.5, %rdi
	movl	%eax, -28(%rbp)         # 4-byte Spill
	callq	puts
	movq	-16(%rbp), %rcx
	movl	%eax, -32(%rbp)         # 4-byte Spill
	movq	%rcx, %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end3:
	.size	ll_ltoa, .Lfunc_end3-ll_ltoa
	.cfi_endproc
                                        # -- End function
	.globl	ll_malloc               # -- Begin function ll_malloc
	.p2align	4, 0x90
	.type	ll_malloc,@function
ll_malloc:                              # @ll_malloc
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	callq	calloc
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end4:
	.size	ll_malloc, .Lfunc_end4-ll_malloc
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"hello from ll_callback"
	.size	.L.str, 23

	.type	.L.str.1,@object        # @.str.1
.L.str.1:
	.asciz	"result from foo: %lu\n"
	.size	.L.str.1, 22

	.type	.L.str.2,@object        # @.str.2
.L.str.2:
	.asciz	"ll_ltoa - entry"
	.size	.L.str.2, 16

	.type	.L.str.3,@object        # @.str.3
.L.str.3:
	.asciz	"ll_ltoa - calloc ok"
	.size	.L.str.3, 20

	.type	.L.str.4,@object        # @.str.4
.L.str.4:
	.asciz	"%ld"
	.size	.L.str.4, 4

	.type	.L.str.5,@object        # @.str.5
.L.str.5:
	.asciz	"ll_ltoa - snprintf ok"
	.size	.L.str.5, 22

	.ident	"clang version 10.0.0-4ubuntu1 "
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym puts
	.addrsig_sym strlen
	.addrsig_sym calloc
	.addrsig_sym strncpy
	.addrsig_sym printf
	.addrsig_sym snprintf
