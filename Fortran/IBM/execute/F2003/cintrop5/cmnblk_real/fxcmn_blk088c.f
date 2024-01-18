!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk_qlngdbl.sh fxcmn_blk088c cxcmn_blk088
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk088c fxcmn_blk088c.out
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
!*                               Data type being tested:  real*16
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
! Real Array Declaration
! ----------------------------------------------------------------------------
         real (kind=o'20')                       :: real_s16c(-2:2) 

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label 
! ----------------------------------------------------------------------------

         common /blk_real_s16c/         real_s16c 

         bind(c, Name='_L_a_b_e_l_4_U') ::   /blk_real_s16c/ 

end module fmod1 


program fxcmn_blk088c
      use fmod1
      implicit none
      logical precision_r4, precision_r8, precision_r16

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         real_s16c                       =  (/1.797693Q+308, -2.225073Q-308, 0.0Q0, 2.225073Q-308, -1.797693Q+308/) 

! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         if ( .not. precision_r16 ( real_s16c(-2)           ,  1.797693Q+308 )) error stop 10 
         if ( .not. precision_r16 ( real_s16c(-1)           , -2.225073Q-308 )) error stop 11 
         if ( .not. precision_r16 ( real_s16c(0)            ,  0.0Q0         )) error stop 12 
         if ( .not. precision_r16 ( real_s16c(1)            ,  2.225073Q-308 )) error stop 13 
         if ( .not. precision_r16 ( real_s16c(2)            , -1.797693Q+308 )) error stop 14 


! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         if ( .not. precision_r16 ( real_s16c(2)            ,  1.797693Q+308 )) error stop 20 
         if ( .not. precision_r16 ( real_s16c(1)            , -2.225073Q-308 )) error stop 21 
         if ( .not. precision_r16 ( real_s16c(0)            ,  0.0Q0         )) error stop 22 
         if ( .not. precision_r16 ( real_s16c(-1)           ,  2.225073Q-308 )) error stop 23 
         if ( .not. precision_r16 ( real_s16c(-2)           , -1.797693Q+308 )) error stop 24 


end program
