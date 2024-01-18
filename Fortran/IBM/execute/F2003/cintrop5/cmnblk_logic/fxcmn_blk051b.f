!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk051b cxcmn_blk051
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk051b fxcmn_blk051b.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : February 13, 2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95, xlc
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This test case will verify that a single variable inside of
!*                               BIND(C) common block with a binding label is interoperable
!*                               with a C variable that is not in a structure.
!*
!*                               Data type being tested: logical(C_BOOL)
!*					
!*                               Test: BIND(C) common block in external subroutine 
!*					
!* ===================================================================
!*  REVISION HISTORY					
!*  MM/DD/YY:  Init:  Comments:			
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk051b
	use iso_c_binding
        implicit none

      call extern_fsub()

End program

subroutine extern_fsub()
	use iso_c_binding
        implicit none

	logical(C_BOOL)  :: log1

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed logical(C_BOOL)s
! ----------------------------------------------------------------------------
        common /blk1/    log1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

        log1  = .true.

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

        !*** Verify values before passing to C
        if ( log1 	.neqv. 	.true. )       		error stop 5 


	!***  Call to C subprogram
	CALL CSUB1()


        !*** Verify values that were modified by C subprogram
        if ( log1 	.neqv. 	.false. )      		error stop 10 

   end subroutine
