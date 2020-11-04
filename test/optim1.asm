	.text
	.file	"Tony program"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	$4, 8(%rsp)
	movq	$4, 16(%rsp)
	movq	$4, (%rsp)
	cmpq	$11, (%rsp)
	jl	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %body
                                        # =>This Inner Loop Header: Depth=1
	decq	8(%rsp)
	incq	(%rsp)
	cmpq	$11, (%rsp)
	jge	.LBB0_2
.LBB0_3:                                # %endfor
	movq	(%rsp), %rdi
	callq	puti
	xorl	%eax, %eax
	addq	$24, %rsp
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
