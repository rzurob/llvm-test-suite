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
!*                               Data type being tested:  INTEGER(C_INTMAX_T)
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

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	INTEGER (C_INTMAX_T 		)		:: int_C_INTMAX_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INTMAX_T/     int_C_INTMAX_T
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INTMAX_T/

end module fmod1


program fxcmn_blk072c
      use fmod1
      implicit none

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_C_INTMAX_T                  = (/9223372036854775807_8,b'000000000',-9223372036854775807_8, o'3641100', -2147483648_8/)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( int_c_intmax_t(1)          .ne.  9223372036854775807_8 )    error stop 10
	if ( int_c_intmax_t(2)          .ne.  b'000000000' 	    )    error stop 11
	if ( int_c_intmax_t(3)          .ne. -9223372036854775807_8 )    error stop 12
	if ( int_c_intmax_t(4)          .ne.  o'3641100'	    )    error stop 13
	if ( int_c_intmax_t(5)          .ne. -2147483648_8 	    )    error stop 14

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( int_c_intmax_t(5)          .ne.  9223372036854775807_8 )    error stop 20
	if ( int_c_intmax_t(4)          .ne.  b'000000000'          )    error stop 21
	if ( int_c_intmax_t(3)          .ne. -9223372036854775807_8 )    error stop 22
	if ( int_c_intmax_t(2)          .ne.  o'3641100'            )    error stop 23
	if ( int_c_intmax_t(1)          .ne. -2147483648_8          )    error stop 24

end program