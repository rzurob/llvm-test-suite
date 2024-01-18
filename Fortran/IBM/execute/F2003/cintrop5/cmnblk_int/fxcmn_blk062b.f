!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk062b cxcmn_blk062
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk062b fxcmn_blk062b.out
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
!*                               Data type being tested:  integer*2
!*
!*                               Test: BIND(C) common block in external subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk062b
	use iso_c_binding
        implicit none

      call extern_fsub()

End program

subroutine extern_fsub()
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	integer (kind=2_4) 				:: int_s2(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement
! ----------------------------------------------------------------------------

	COMMON     /blk_int_s2/            int_s2
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_s2/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_s2                  =  (/o'77777',o'0',-32768, 32767,b'1111111'/)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( int_s2(1)        	.ne.  o'77777'      		)    error stop 10
	if ( int_s2(2)        	.ne.  o'0'       		)    error stop 11
	if ( int_s2(3)        	.ne.  -32768      		)    error stop 12
	if ( int_s2(4)        	.ne.  32767      		)    error stop 13
	if ( int_s2(5)        	.ne.  b'1111111'      		)    error stop 14

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( int_s2(5)         .ne.  o'77777'                  )    error stop 20
	if ( int_s2(4)         .ne.  o'0'                      )    error stop 21
	if ( int_s2(3)         .ne.  -32768                    )    error stop 22
	if ( int_s2(2)         .ne.  32767                     )    error stop 23
	if ( int_s2(1)         .ne.  b'1111111'                )    error stop 24

   end subroutine
