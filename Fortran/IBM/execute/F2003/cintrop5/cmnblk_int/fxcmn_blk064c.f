!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk064c cxcmn_blk064
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk064c fxcmn_blk064c.out
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
!*                               Data type being tested:  integer*8
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


	integer (kind=int((4.4e0_8,6.5e0_8))+4 ) 	:: int_s8(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement  
! ----------------------------------------------------------------------------

	COMMON     /blk_int_s8/            int_s8      		
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_s8/    

end module fmod1 


program fxcmn_blk064c
      use fmod1
      implicit none

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_s8                  =  (/9223372036854775807_8,b'000000000',-9223372036854775807_8, o'3641100', -2147483648_8/)  

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( int_s8(1)         .ne.  9223372036854775807_8 	)    error stop 10
	if ( int_s8(2)         .ne.  b'000000000' 		)    error stop 11
	if ( int_s8(3)         .ne. -9223372036854775807_8 	)    error stop 12
	if ( int_s8(4)         .ne.  o'3641100' 		)    error stop 13
	if ( int_s8(5)         .ne. -2147483648_8   		)    error stop 14

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( int_s8(5)         .ne.  9223372036854775807_8     )    error stop 20
	if ( int_s8(4)         .ne.  b'000000000'              )    error stop 21
	if ( int_s8(3)         .ne. -9223372036854775807_8     )    error stop 22
	if ( int_s8(2)         .ne.  o'3641100'                )    error stop 23
	if ( int_s8(1)         .ne. -2147483648_8              )    error stop 24

end program
