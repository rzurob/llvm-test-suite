!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk070 cxcmn_blk070
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk070 fxcmn_blk070.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : March 19, 2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95, xlc, gcc 
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that 1-dimensional array 
!*				 variables inside of common blocks are interoperable 
!*				 with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  INTEGER(C_SIZE_T)
!*
!*				 Scope being tested:  main program
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk070
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	INTEGER (C_SIZE_T 		)		:: int_C_SIZE_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement  
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_SIZE_T/       int_C_SIZE_T        
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_SIZE_T/     

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_C_SIZE_T                    = (/2147483647,b'1111111',-2147483648, 0, o'3641100'/)      

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( int_c_size_t(1)            .ne.  2147483647            )    error stop 10
	if ( int_c_size_t(2)            .ne.  b'1111111'            )    error stop 11
	if ( int_c_size_t(3)            .ne. -2147483648            )    error stop 12
	if ( int_c_size_t(4)            .ne.  0            	    )    error stop 13
	if ( int_c_size_t(5)            .ne.  o'3641100'            )    error stop 14

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( int_c_size_t(5)            .ne.  2147483647            )    error stop 20
	if ( int_c_size_t(4)            .ne.  b'1111111'            )    error stop 21
	if ( int_c_size_t(3)            .ne.  2147483647            )    error stop 22
	if ( int_c_size_t(2)            .ne.  0                     )    error stop 23
	if ( int_c_size_t(1)            .ne.  o'3641100'            )    error stop 24

end program
