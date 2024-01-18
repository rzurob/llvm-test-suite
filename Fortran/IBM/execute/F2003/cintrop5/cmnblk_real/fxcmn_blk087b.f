!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk_qlngdbl.sh fxcmn_blk087b cxcmn_blk087
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk087b fxcmn_blk087b.out
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
!*                               Data type being tested:  real*8
!*					
!*                               Test: BIND(C) common block in external subroutine 
!*					
!* ===================================================================
!*  REVISION HISTORY					
!*  MM/DD/YY:  Init:  Comments:			
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk087b
	use iso_c_binding
        implicit none

      call extern_fsub()

End program

subroutine extern_fsub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i
        integer*4,parameter     :: N=5


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         real (kind=INT((4.4e0_8,6.5e0_8))+4 )   :: real_s8b(N) 

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label 
! ----------------------------------------------------------------------------

         common /blk_real_s8b/          real_s8b 

         bind(c, Name='_L_a_b_e_l_4_U') ::   /blk_real_s8b/ 

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         real_s8b                        =  (/1.797693D+308, -2.5D-294, 0.0D0, 2.5D-294, -1.797693D+308/) 

! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         if ( .not. precision_r8 ( real_s8b(1)              ,  1.797693D+308 )) error stop 10 
         if ( .not. precision_r8 ( real_s8b(2)              , -2.5D-294 )) error stop 11 
         if ( .not. precision_r8 ( real_s8b(3)              ,  0.0D0         )) error stop 12 
         if ( .not. precision_r8 ( real_s8b(4)              ,  2.5D-294 )) error stop 13 
         if ( .not. precision_r8 ( real_s8b(5)              , -1.797693D+308 )) error stop 14 


! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         if ( .not. precision_r8 ( real_s8b(5)              ,  1.797693D+308 )) error stop 20 
         if ( .not. precision_r8 ( real_s8b(4)              , -2.5D-294 )) error stop 21 
         if ( .not. precision_r8 ( real_s8b(3)              ,  0.0D0         )) error stop 22 
         if ( .not. precision_r8 ( real_s8b(2)              ,  2.5D-294 )) error stop 23 
         if ( .not. precision_r8 ( real_s8b(1)              , -1.797693D+308 )) error stop 24 


   end subroutine
