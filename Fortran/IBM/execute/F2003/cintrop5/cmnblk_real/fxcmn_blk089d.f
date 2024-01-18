!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk_qlngdbl.sh fxcmn_blk089d cxcmn_blk089
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk089d fxcmn_blk089d.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : February 13, 2004
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
!*                               variables inside of common blocks are interoperable
!*                               with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  REAL(C_FLOAT)
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
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i
        integer*4,parameter     :: N=5


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         REAL (C_FLOAT   ), DIMENSION(-5:-1)     :: r_C_FLOAT_s4a 

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label 
! ----------------------------------------------------------------------------

         common /blk_r_C_FLOAT_s4a/     r_C_FLOAT_s4a 

         bind(c, Name='_L_a_b_e_l_4_U') ::   /blk_r_C_FLOAT_s4a/ 

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         r_C_FLOAT_s4a                   =  (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/) 

! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-5)        ,  3.402823E+38  )) error stop 10 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-4)        , -1.175494E-38  )) error stop 11 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-3)        ,  0.0           )) error stop 12 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-2)        ,  1.175494E-38  )) error stop 13 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-1)        , -3.402823E+38  )) error stop 14 


! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-1)        ,  3.402823E+38  )) error stop 20 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-2)        , -1.175494E-38  )) error stop 21 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-3)        ,  0.0           )) error stop 22 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-4)        ,  1.175494E-38  )) error stop 23 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-5)        , -3.402823E+38  )) error stop 24 


     end subroutine

end module fmod1 



program fxcmn_blk089d
	use fmod1 
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i
        integer*4,parameter     :: N=5


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         REAL (C_FLOAT   ), DIMENSION(-5:-1)     :: r_C_FLOAT_s4a 

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label 
! ----------------------------------------------------------------------------

         common /blk_r_C_FLOAT_s4a/     r_C_FLOAT_s4a 

         bind(c, Name='_L_a_b_e_l_4_U') ::   /blk_r_C_FLOAT_s4a/ 

	!*** Call module subroutine
	call Intern_FSub()

! ---------------------------------------------------------------------------- 
! Real Verification 
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-1)        ,  3.402823E+38  )) error stop 50 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-2)        , -1.175494E-38  )) error stop 51 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-3)        ,  0.0           )) error stop 52 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-4)        ,  1.175494E-38  )) error stop 53 
         if ( .not. precision_r4 ( r_C_FLOAT_s4a(-5)        , -3.402823E+38  )) error stop 54 


end program
