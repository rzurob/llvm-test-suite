!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk079d cxcmn_blk079
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk079d fxcmn_blk079d.out
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
!*                               Data type being tested:  INTEGER(C_INT_LEAST32_T)
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


	INTEGER (C_INT_LEAST32_T 	)		:: int_C_INT_LEAST32_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement  
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INT_LEAST32_T/ int_C_INT_LEAST32_T
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INT_LEAST32_T/   

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_C_INT_LEAST32_T             = 0      

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( ANY (int_c_int_least32_t(1:5) .ne.  0                  ))    error stop 10

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( ANY (int_c_int_least32_t(1:5) .ne.  2147483647         ))   error stop 20

     end subroutine

end module fmod1 



program fxcmn_blk079d
	use fmod1 
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	INTEGER (C_INT_LEAST32_T 	)		:: int_C_INT_LEAST32_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement  
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INT_LEAST32_T/ int_C_INT_LEAST32_T
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INT_LEAST32_T/   

	!*** Call module subroutine
	call Intern_FSub()

! ---------------------------------------------------------------------------- 
! Integer Verification 
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

	if ( ANY (int_c_int_least32_t(1:5) .ne.  2147483647         ))   error stop 50

end program
