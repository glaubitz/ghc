#include "ghcconfig.h"
#include "rts/Constants.h"

#if defined(powerpc64le_HOST_ARCH)
# ifdef linux_HOST_OS
# define STACK_FRAME_SIZE RESERVED_C_STACK_BYTES+304
	.file	"StgCRun.c"
	.abiversion 2
	.section	".toc","aw"
	.section	".text"
	.align 2
.globl StgRun
.hidden StgRun
.type StgRun,@function
StgRun:
.localentry StgRun,.-StgRun
	mflr 0
	mr 5, 1
	std 0, 16(1)
	stdu 1, -(STACK_FRAME_SIZE)(1)
	std 2, -296(5)
	std 14, -288(5)
	std 15, -280(5)
	std 16, -272(5)
	std 17, -264(5)
	std 18, -256(5)
	std 19, -248(5)
	std 20, -240(5)
	std 21, -232(5)
	std 22, -224(5)
	std 23, -216(5)
	std 24, -208(5)
	std 25, -200(5)
	std 26, -192(5)
	std 27, -184(5)
	std 28, -176(5)
	std 29, -168(5)
	std 30, -160(5)
	std 31, -152(5)
	stfd 14, -144(5)
	stfd 15, -136(5)
	stfd 16, -128(5)
	stfd 17, -120(5)
	stfd 18, -112(5)
	stfd 19, -104(5)
	stfd 20, -96(5)
	stfd 21, -88(5)
	stfd 22, -80(5)
	stfd 23, -72(5)
	stfd 24, -64(5)
	stfd 25, -56(5)
	stfd 26, -48(5)
	stfd 27, -40(5)
	stfd 28, -32(5)
	stfd 29, -24(5)
	stfd 30, -16(5)
	stfd 31, -8(5)
	mr 27, 4
	mtctr 3
	mr 12, 3
	bctr
.globl StgReturn
.type StgReturn,@function
StgReturn:
.localentry StgReturn,.-StgReturn
	mr 3,14
	la 5, STACK_FRAME_SIZE(1)
	ld 2, -296(5)
	ld 14, -288(5)
	ld 15, -280(5)
	ld 16, -272(5)
	ld 17, -264(5)
	ld 18, -256(5)
	ld 19, -248(5)
	ld 20, -240(5)
	ld 21, -232(5)
	ld 22, -224(5)
	ld 23, -216(5)
	ld 24, -208(5)
	ld 25, -200(5)
	ld 26, -192(5)
	ld 27, -184(5)
	ld 28, -176(5)
	ld 29, -168(5)
	ld 30, -160(5)
	ld 31, -152(5)
	lfd 14, -144(5)
	lfd 15, -136(5)
	lfd 16, -128(5)
	lfd 17, -120(5)
	lfd 18, -112(5)
	lfd 19, -104(5)
	lfd 20, -96(5)
	lfd 21, -88(5)
	lfd 22, -80(5)
	lfd 23, -72(5)
	lfd 24, -64(5)
	lfd 25, -56(5)
	lfd 26, -48(5)
	lfd 27, -40(5)
	lfd 28, -32(5)
	lfd 29, -24(5)
	lfd 30, -16(5)
	lfd 31, -8(5)
	mr 1, 5
	ld 0, 16(1)
	mtlr 0
	blr

	.section	.note.GNU-stack,"",@progbits
# else // linux_HOST_OS
# error Only Linux support for power64 little endian right now.
# endif

#elif defined(powerpc_HOST_ARCH)
# if defined(aix_HOST_OS)
# define STACK_FRAME_SIZE RESERVED_C_STACK_BYTES+224
	.toc
	.csect StgRun[DS]
	.globl StgRun[DS]
	.long .StgRun, TOC[TC0], 0
	.csect .text[PR]
	.globl .StgRun
.StgRun:
	mflr  0
	mr    5,1
	stw   0,8(1)
	stwu  1,-(STACK_FRAME_SIZE)(1)
	stw   2,-224(5)
	stmw 13,-220(5)
	stfd 14,-144(5)
	stfd 15,-136(5)
	stfd 16,-128(5)
	stfd 17,-120(5)
	stfd 18,-112(5)
	stfd 19,-104(5)
	stfd 20,-96(5)
	stfd 21,-88(5)
	stfd 22,-80(5)
	stfd 23,-72(5)
	stfd 24,-64(5)
	stfd 25,-56(5)
	stfd 26,-48(5)
	stfd 27,-40(5)
	stfd 28,-32(5)
	stfd 29,-24(5)
	stfd 30,-16(5)
	stfd 31,-8(5)
	mr   27,4
	mtctr 3
	bctr

	.globl StgReturn
StgReturn:
	mr   3,14
	la   5,(STACK_FRAME_SIZE)(1)
	lwz  2,-224(5)
	lmw 13,-220(5)
	lfd 14,-144(5)
	lfd 15,-136(5)
	lfd 16,-128(5)
	lfd 17,-120(5)
	lfd 18,-112(5)
	lfd 19,-104(5)
	lfd 20,-96(5)
	lfd 21,-88(5)
	lfd 22,-80(5)
	lfd 23,-72(5)
	lfd 24,-64(5)
	lfd 25,-56(5)
	lfd 26,-48(5)
	lfd 27,-40(5)
	lfd 28,-32(5)
	lfd 29,-24(5)
	lfd 30,-16(5)
	lfd 31,-8(5)
	mr   1,5
	lwz  0,8(1)
	mtlr 0
	blr

# endif // aix_HOST_OS

/* -----------------------------------------------------------------------------
   Sparc architecture

   --
   OLD COMMENT from GHC-3.02:

   We want tailjumps to be calls, because `call xxx' is the only Sparc
   branch that allows an arbitrary label as a target.  (Gcc's ``goto
   *target'' construct ends up loading the label into a register and
   then jumping, at the cost of two extra instructions for the 32-bit
   load.)

   When entering the threaded world, we stash our return address in a
   known location so that \tr{%i7} is available as an extra
   callee-saves register.  Of course, we have to restore this when
   coming out of the threaded world.

   I hate this god-forsaken architecture.  Since the top of the
   reserved stack space is used for globals and the bottom is reserved
   for outgoing arguments, we have to stick our return address
   somewhere in the middle.  Currently, I'm allowing 100 extra
   outgoing arguments beyond the first 6.  --JSM

   Updated info (GHC 4.06): we don't appear to use %i7 any more, so
   I'm not sure whether we still need to save it.  Incedentally, what
   does the last paragraph above mean when it says "the top of the
   stack is used for globals"?  What globals?  --SDM

   Updated info (GHC 4.08.2): not saving %i7 any more (see below).

   Updated info (GHC 8.0.1): Rewritten in assembly since %l7 stores the GOT
   address, but it gets clobbered and GCC doesn't like that, so let's not
   fight it.
   -------------------------------------------------------------------------- */

#elif defined(sparc_HOST_ARCH) || defined(sparc64_HOST_ARCH)
# if defined(linux_HOST_OS) || defined(solaris_HOST_OS)
	.file	"StgCRun.c"
	.section	".text"
	.align 4
	.global StgRun
	.hidden StgRun
	.type StgRun,@function
StgRun:
#  if defined(sparc64_HOST_ARCH)
#   define STACK_FRAME_SIZE (RESERVED_C_STACK_BYTES+176)
	sethi %hix(-STACK_FRAME_SIZE), %o2
	xor %o2, %lox(-STACK_FRAME_SIZE), %o2
#  else
#   define STACK_FRAME_SIZE (RESERVED_C_STACK_BYTES+96)
	sethi %hi(-STACK_FRAME_SIZE), %o2
	or %o2, %lo(-STACK_FRAME_SIZE), %o2
#  endif // sparc64_HOST_ARCH
	save %sp, %o2, %sp
	// Call f
	jmpl %i0, %o7
	 nop

	.global StgReturn
StgReturn:
	//ldx [%i1], %i0 // load R1 into %i0 for return value
	//or %g0, %l1, %i0 // load R1 into %i0 for return value
	ret
	 restore %g0, %l1, %o0 // load R1 into caller's %o0 for return value

#  if defined(linux_HOST_OS)
	.section	.note.GNU-stack,"",@progbits
#  endif

# else // linux_HOST_OS || solaris_HOST_OS
# error Only Linux and Solaris are supported for sparc and sparc64 right now.
# endif
#endif
