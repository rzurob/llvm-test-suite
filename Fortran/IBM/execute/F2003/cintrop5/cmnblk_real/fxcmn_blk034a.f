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
!*                               Data type being tested: real*16
!*
!*                               Test: BIND(C) common block in internal subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk034a
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

	real (kind=MIN(16,19)) 	:: Real1

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    Real1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

        !-- commented out TINY(Real1), because it gives 0.20041683600089727779961080513501620E-291
        !-- Real1  =  -( TINY(Real1) )

        Real1  = -2.225073Q-308

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

        !*** Verify values before passing to C
        if ( .not. precision_r16 ( Real1     ,  -2.225073Q-308  )) error stop 5

	!***  Call to C subprogram
	CALL CSUB_REAL()


        !*** Verify values that were modified by C subprogram
        if ( .not. precision_r16 ( Real1     ,  1.797693Q+308  )) error stop 10

   end subroutine

end program