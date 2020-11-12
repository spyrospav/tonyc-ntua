	.text
	.file	"Tony program"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	$1, (%rsp)
	cmpq	$41, (%rsp)
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsp), %rdi
	callq	puti
	movl	$10, %edi
	callq	putc
	incq	(%rsp)
	cmpq	$41, (%rsp)
	jle	.LBB0_2
.LBB0_3:                                # %endfor
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lnl,@object            # @nl
	.section	.rodata,"a",@progbits
.Lnl:
	.asciz	"\n"
	.size	.Lnl, 2

	.section	".note.GNU-stack","",@progbits
