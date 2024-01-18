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
!*                               Data type being tested: real*8
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

	real (kind=INT((4.4e0_8,6.5e0_8))+4 ) 	:: Real1

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    Real1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

end module fmod1


program fxcmn_blk033c
      use fmod1
      implicit none
      logical precision_r4, precision_r8, precision_r16

        Real1  =  -( TINY(Real1) )

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

!        Issue with the conversion of the literal into a binary number
!        !*** Verify values before passing to C; camparing with precision_r8 will cause comparison to fail.
!        if ( .not. precision_r4 ( Real1     ,  -2.225073D-308  )) error stop 5


	!***  Call to C subprogram
	CALL CSUB_REAL()


        !*** Verify values that were modified by C subprogram
        if ( .not. precision_r8 ( Real1     ,  1.797693D+308  )) error stop 10

end program
