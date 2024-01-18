!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk061d cxcmn_blk061
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk061d fxcmn_blk061d.out
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
!*                               Data type being tested:  integer*1
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

end module fmod1 



program fxcmn_blk061d
	use fmod1 
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

	!*** Call module subroutine
	call Intern_FSub()

! ---------------------------------------------------------------------------- 
! Integer Verification 
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

	if ( int_s1(5)         .ne.  b'1111111'                )    error stop 50
	if ( int_s1(4)         .ne.  o'0'                      )    error stop 51
	if ( int_s1(3)         .ne.  -128                      )    error stop 52
	if ( int_s1(2)         .ne.  o'177'                    )    error stop 53
	if ( int_s1(1)         .ne.  127                       )    error stop 54

end program
