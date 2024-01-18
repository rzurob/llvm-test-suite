!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk083 cxcmn_blk083
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk083 fxcmn_blk083.out
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
!*                               Data type being tested:  INTEGER(C_INT_FAST32_T)
!*
!*				 Scope being tested:  main program
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk083
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	INTEGER (C_INT_FAST32_T 	)		:: int_C_INT_FAST32_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INT_FAST32_T/  int_C_INT_FAST32_T
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INT_FAST32_T/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_C_INT_FAST32_T              = (/2147483647,b'1111111',-2147483648, 0, o'3641100'/)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( int_c_int_fast32_t(1)    .ne.  2147483647              )    error stop 10
	if ( int_c_int_fast32_t(2)    .ne.  b'1111111'              )    error stop 11
	if ( int_c_int_fast32_t(3)    .ne. -2147483648              )    error stop 12
	if ( int_c_int_fast32_t(4)    .ne.  0            	    )    error stop 13
	if ( int_c_int_fast32_t(5)    .ne.  o'3641100'              )    error stop 14

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( int_c_int_fast32_t(5)    .ne.  2147483647              )    error stop 20
	if ( int_c_int_fast32_t(4)    .ne.  b'1111111'              )    error stop 21
	if ( int_c_int_fast32_t(3)    .ne. -2147483648              )    error stop 22
	if ( int_c_int_fast32_t(2)    .ne.  0                       )    error stop 23
	if ( int_c_int_fast32_t(1)    .ne.  o'3641100'              )    error stop 24

end program
