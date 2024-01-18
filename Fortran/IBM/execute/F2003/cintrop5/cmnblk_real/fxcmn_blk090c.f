!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 13, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that 1-dimensional array
!*                               variables inside of common blocks are interoperable
!*                               with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  REAL(C_DOUBLE)
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

        integer*4               :: i

! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         REAL (C_DOUBLE  )                       :: r_C_DOUBLE_s8c(-2:2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_r_C_DOUBLE_s8c/    r_C_DOUBLE_s8c

         bind(c, Name='_L_a_b_e_l_4_U') ::   /blk_r_C_DOUBLE_s8c/

end module fmod1


program fxcmn_blk090c
      use fmod1
      implicit none
      logical precision_r4, precision_r8, precision_r16

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         r_C_DOUBLE_s8c                  = -1.797693D+308

! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         do i = -2, 2
            if ( .not. precision_r8 ( r_C_DOUBLE_s8c(i)    , -1.797693D+308 )) call zzrc(12+i)
         end do


! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         do i = -2, 2
            if ( .not. precision_r8 ( r_C_DOUBLE_s8c(i)    ,  1.797693D+308 )) call zzrc(22+i)
         end do


end program
