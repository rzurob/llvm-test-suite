!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk072d cxcmn_blk072
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk072d fxcmn_blk072d.out
! %END
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


	INTEGER (C_INTMAX_T 		)		:: int_C_INTMAX_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INTMAX_T/     int_C_INTMAX_T
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INTMAX_T/

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

     end subroutine

end module fmod1



program fxcmn_blk072d
	use fmod1
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

	!*** Call module subroutine
	call Intern_FSub()

! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

	if ( int_c_intmax_t(5)          .ne.  9223372036854775807_8 )    error stop 50
	if ( int_c_intmax_t(4)          .ne.  b'000000000'          )    error stop 51
	if ( int_c_intmax_t(3)          .ne. -9223372036854775807_8 )    error stop 52
	if ( int_c_intmax_t(2)          .ne.  o'3641100'            )    error stop 53
	if ( int_c_intmax_t(1)          .ne. -2147483648_8          )    error stop 54

end program
