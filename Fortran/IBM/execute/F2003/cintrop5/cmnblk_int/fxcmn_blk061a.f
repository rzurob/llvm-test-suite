!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk061a cxcmn_blk061
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk061a fxcmn_blk061a.out
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
!*                               Data type being tested:  integer*1
!*
!*                               Test: BIND(C) common block in internal subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk061a
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	integer (kind=o'001')				:: int_s1(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement
! ----------------------------------------------------------------------------

	COMMON     /blk_int_s1/     	int_s1
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_s1/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_s1                  =  (/b'1111111',o'0',-128,o'177',127/)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( int_s1(1)        	.ne.  b'1111111'      		)    error stop 10
	if ( int_s1(2)        	.ne.  o'0'       		)    error stop 11
	if ( int_s1(3)        	.ne.  -128       		)    error stop 12
	if ( int_s1(4)        	.ne.  o'177'      		)    error stop 13
	if ( int_s1(5)        	.ne.  127       		)    error stop 14

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( int_s1(5)         .ne.  b'1111111'                )    error stop 20
	if ( int_s1(4)         .ne.  o'0'                      )    error stop 21
	if ( int_s1(3)         .ne.  -128                      )    error stop 22
	if ( int_s1(2)         .ne.  o'177'                    )    error stop 23
	if ( int_s1(1)         .ne.  127                       )    error stop 24

   end subroutine

end program
