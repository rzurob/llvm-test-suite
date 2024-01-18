!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk_Q64.sh fxcmn_blk011q64b cxcmn_blk011q64
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fxcmn_blk011q64b.o cxcmn_blk011q64.o fxcmn_blk011q64b
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
!*                               Data type being tested: INTEGER (C_SIZE_T)  with -q64
!*
!*                               Test: BIND(C) common block in external subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk011q64b
	use iso_c_binding
        implicit none

      call extern_fsub()

End program

subroutine extern_fsub()
	use iso_c_binding
        implicit none

        INTEGER (C_SIZE_T   )   :: 	int1


! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    int1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

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

   end subroutine
