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
!*                               Data type being tested:  REAL(C_LONG_DOUBLE)
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
         REAL (C_LONG_DOUBLE ), DIMENSION(11:15) :: r_C_LONG_DOUBLE_s8a

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_r_C_LONG_DOUBLE_s8a/          r_C_LONG_DOUBLE_s8a

         bind(c, Name='_L_a_b_e_l_4_U') ::  /blk_r_C_LONG_DOUBLE_s8a/

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         r_C_LONG_DOUBLE_s8a    =  (/1.797693Q+308, -2.004168Q-292, 0.0Q0, 2.004168Q-292, -1.797693Q+308/)

! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(11)  ,  1.797693Q+308 )) error stop 10
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(12)  , -2.004168Q-292 )) error stop 11
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(13)  ,  0.0Q0         )) error stop 12
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(14)  ,  2.004168Q-292 )) error stop 13
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(15)  , -1.797693Q+308 )) error stop 14


! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(15)  ,  1.797693Q+308 )) error stop 10
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(14)  , -2.004168Q-292 )) error stop 11
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(13)  ,  0.0Q0         )) error stop 12
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(12)  ,  2.004168Q-292 )) error stop 13
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(11)  , -1.797693Q+308 )) error stop 14


     end subroutine

end module fmod1



program fxcmn_blk091d
	use fmod1
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i
        integer*4,parameter     :: N=5


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         REAL (C_LONG_DOUBLE ), DIMENSION(11:15) :: r_C_LONG_DOUBLE_s8a

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_r_C_LONG_DOUBLE_s8a/          r_C_LONG_DOUBLE_s8a

         bind(c, Name='_L_a_b_e_l_4_U') ::  /blk_r_C_LONG_DOUBLE_s8a/

	!*** Call module subroutine
	call Intern_FSub()

! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(15)  ,  1.797693Q+308 )) error stop 50
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(14)  , -2.004168Q-292 )) error stop 51
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(13)  ,  0.0Q0         )) error stop 52
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(12)  ,  2.004168Q-292 )) error stop 53
         if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a(11)  , -1.797693Q+308 )) error stop 54


end program
