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
!*                               Data type being tested: character
!*
!*                               Test: BIND(C) common block inside a module subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module fmod1
   implicit none

   CONTAINS
     subroutine Intern_FSub()
	use iso_c_binding
        implicit none

	character  :: char1

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    char1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

        char1  =  'A'

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

        !*** Verify values before passing to C
        if ( char1 	.ne. 	'A' )       		error stop 5


	!***  Call to C subprogram
	CALL CSUB1()


        !*** Verify values that were modified by C subprogram
        if ( char1 	.ne. 	'Z' )       		error stop 10

     end subroutine

end module fmod1



program fxcmn_blk041d
	use fmod1
	use iso_c_binding
        implicit none

	character  :: char1

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    char1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

	!*** Call module subroutine
	call Intern_FSub()

        !*** Verify values that were passed from module subroutine
        if ( char1 	.ne. 	'Z' )       		error stop 50

end program
