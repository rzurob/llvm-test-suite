!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 19, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that 1-dimensional array
!*				 variables inside of common blocks are interoperable
!*				 with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  INTEGER(C_INT8_T)
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

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	INTEGER (C_INT8_T 		)		:: int_C_INT8_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INT8_T/       int_C_INT8_T
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INT8_T/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_C_INT8_T                    = -128

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( ANY (int_c_int8_t(1:5)     .ne. -128                   ))   error stop 10

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( ANY (int_c_int8_t(1:5)     .ne. 127                   ))   error stop 20

     end subroutine

end module fmod1



program fxcmn_blk073d
	use fmod1
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	INTEGER (C_INT8_T 		)		:: int_C_INT8_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INT8_T/       int_C_INT8_T
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INT8_T/

	!*** Call module subroutine
	call Intern_FSub()

! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

	if ( ANY (int_c_int8_t(1:5)     .ne. 127                   ))   error stop 50

end program
