!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 13, 2004
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that a single variable inside of
!*                               BIND(C) common block with a binding label is interoperable
!*                               with a C variable that is not in a structure.
!*
!*                               Data type being tested: REAL (C_LONG_DOUBLE)
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk037
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

	REAL (C_LONG_DOUBLE )	:: Real1

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    Real1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

        Real1  =  -( TINY(Real1) )

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

print *,Real1
        !*** Verify values before passing to
!        if (.not. precision_r16(Real1,  -0.2004168360008972777996108051350Q-291)) error stop 5
        if (.not. precision_r8(Real1, -0.20041683600089728D-291)) error stop 5



	!***  Call to C subprogram
	CALL CSUB_REAL()

print *,Real1
        !*** Verify values that were modified by C subprogram
!        if ( .not. precision_r16 ( Real1, 1.7976931348623158079372897140530120Q+308)) error stop 10
        if ( .not. precision_r8 ( Real1, 1.7976931348623157D+308)) error stop 10

end program

