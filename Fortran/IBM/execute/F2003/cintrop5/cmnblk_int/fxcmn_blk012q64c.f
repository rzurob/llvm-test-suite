!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk_Q64.sh fxcmn_blk012q64c cxcmn_blk012q64
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fxcmn_blk012q64c.o cxcmn_blk012q64.o fxcmn_blk012q64c
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 01, 2004
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : This test case will verify that a single variable inside of
!*                               BIND(C) common block with a binding label is interoperable
!*                               with a C variable that is not in a structure.
!*
!*                               Data type being tested: INTEGER (C_INTPTR_T ) with -q64
!*
!*                               Test: BIND(C) common block in module
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module fmod1
	use iso_c_binding
        implicit none

        INTEGER (C_INTPTR_T )   :: 	int1


! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    int1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

end module fmod1


program fxcmn_blk012q64c
      use fmod1
      implicit none

	int1  = -( HUGE(int1) ) - 1


! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

        !*** Verify values before passing to C
        if ( int1 .ne. -9223372036854775808_8 )                         error stop 5

        !*** Call C subroutine
        call csub1()

        !*** Verify values that were modified by C subroutine
        if ( int1 .ne. 9223372036854775807_8 )                          error stop 10


end program
