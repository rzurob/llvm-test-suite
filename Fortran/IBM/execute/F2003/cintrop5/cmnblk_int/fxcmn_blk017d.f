!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk017d cxcmn_blk017
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fxcmn_blk017d.o cxcmn_blk017.o fxcmn_blk017d
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : March 01, 2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95, xlc
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : This test case will verify that a single variable inside of 
!*                               BIND(C) common block with a binding label is interoperable 
!*                               with a C variable that is not in a structure.
!*				 
!*                               Data type being tested: INTEGER (C_INT64_T  )  
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

        INTEGER (C_INT64_T  )   :: 	int1 


! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    int1                                                                                             
        bind(c, name='Bnd_Lbl15') ::       /blk1/                        

	int1  = -( HUGE(int1) )
	

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program 
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

        !*** Verify values before passing to C
        if ( int1 .ne. -9223372036854775807_8 )                       	error stop 5

	!*** Call C subroutine
	call csub1()

        !*** Verify values that were modified by C subroutine
        if ( int1 .ne. 9223372036854775807_8 )                         	error stop 10

     end subroutine

end module fmod1 



program fxcmn_blk017d
	use fmod1 
	use iso_c_binding
        implicit none

        INTEGER (C_INT64_T  )   :: 	int1 


! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    int1                                                                                             
        bind(c, name='Bnd_Lbl15') ::       /blk1/                        

	!*** Call module subroutine
	call Intern_FSub()

        !*** Verify values that were passed from module subroutine
        if ( int1 .ne. 9223372036854775807_8 )                         	error stop 50

end program
