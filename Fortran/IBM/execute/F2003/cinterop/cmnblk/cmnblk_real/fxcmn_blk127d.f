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
!*  DESCRIPTION                : This test case will verify that 3-dimensional array
!*                               variables inside of common blocks are interoperable
!*                               with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  real*16
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

   !*** Resultant Matrix
     real(16)        :: res_real_s16(8)

   !*** Comparison and Temporary Matrix
     real(16)        :: cmp_real_s16(8), tmp_real_s16(0:1,0:1,0:1)


   CONTAINS
     subroutine Intern_FSub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i, j, k
        integer*4,parameter     :: N=2


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         real (kind=o'20')                       :: real_s16(0:1,0:1,0:1)        !* real*16


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_real_s16/         real_s16

         bind(c, Name='_1') ::   /blk_real_s16/

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         real_s16        =  RESHAPE( (/1.797693Q+308, -2.225073Q-308, 0.0Q0, 2.225073Q-308, -1.797693Q+308/), (/2,2,2/), (/1.0Q0,-1.0Q0,-2.9Q0/))

         cmp_real_s16         = (/1.797693Q+308, -2.225073Q-308, 0.0Q0, 2.225073Q-308, -1.797693Q+308, 1.0Q0, -1.0Q0, -2.9Q0/)


! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_real_s16         = RESHAPE(real_s16, (/8/))

         do i = 1, 8

            if ( .not. precision_r16(res_real_s16(i)     ,  cmp_real_s16(i) ))           call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = -2, -1
            do j = -2, -1
               do i = -2, -1
                 tmp_real_s16(i+2,j+2,k+2)       = real_s16(k+2,j+2,i+2)
             end do
           end do
         end do

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         cmp_real_s16            = RESHAPE( tmp_real_s16, (/8/))

         res_real_s16         = RESHAPE(real_s16, (/8/))

         do i = 1, 8

            if ( .not. precision_r16(res_real_s16(i)     ,  cmp_real_s16(i) ))           call zzrc(20+i)

         end do

     end subroutine

end module fmod1



program fxcmn_blk127d
	use fmod1
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i, j, k
        integer*4,parameter     :: N=2


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         real (kind=o'20')                       :: real_s16(0:1,0:1,0:1)        !* real*16


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_real_s16/         real_s16

         bind(c, Name='_1') ::   /blk_real_s16/

	!*** Call module subroutine
	call Intern_FSub()

! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

         res_real_s16         = RESHAPE(real_s16, (/8/))

         do i = 1, 8

            if ( .not. precision_r16(res_real_s16(i)     ,  cmp_real_s16(i) ))           call zzrc(50+i)

         end do

end program
