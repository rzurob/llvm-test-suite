!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk128d cxcmn_blk128
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk128d fxcmn_blk128d.out
! %END
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

   integer*4,parameter     :: N=2

   !*** Resultant Matrix
     REAL            :: res_r_c_float_s4(8)

   !*** Comparison and Temporary Matrix
     REAL            :: cmp_r_c_float_s4(8), tmp_r_c_float_s4(N,N,N)



   CONTAINS
     subroutine Intern_FSub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i, j, k


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         REAL (c_float), DIMENSION(N,N,N)        :: r_c_float_s4


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_r_C_FLOAT_s4/     r_C_FLOAT_s4

         bind(c, Name='_1') ::   /blk_r_C_FLOAT_s4/

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         r_c_float_s4    =  RESHAPE( (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/), (/2,2,2/), (/1.0,-1.0,-2.9/))

         cmp_r_c_float_s4     = (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38,1.0, -1.0, -2.9/)


! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_r_c_float_s4     = RESHAPE(r_c_float_s4, (/8/))


         do i = 1, 8

            if ( .not. precision_r4( res_r_c_float_s4(i) ,  cmp_r_c_float_s4(i) ))       call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = -2, -1
            do j = -2, -1
               do i = -2, -1
                 tmp_r_c_float_s4(i+3,j+3,k+3)   = r_c_float_s4(k+3,j+3,i+3)
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

         cmp_r_c_float_s4        = RESHAPE( tmp_r_c_float_s4, (/8/))

         res_r_c_float_s4     = RESHAPE(r_c_float_s4, (/8/))

         do i = 1, 8

            if ( .not. precision_r4( res_r_c_float_s4(i) ,  cmp_r_c_float_s4(i) ))       call zzrc(20+i)

         end do

     end subroutine

end module fmod1



program fxcmn_blk128d
	use fmod1
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i, j, k


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         REAL (c_float), DIMENSION(N,N,N)        :: r_c_float_s4


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_r_C_FLOAT_s4/     r_C_FLOAT_s4

         bind(c, Name='_1') ::   /blk_r_C_FLOAT_s4/

	!*** Call module subroutine
	call Intern_FSub()

! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

         do i = 1, 8

            if ( .not. precision_r4( res_r_c_float_s4(i) ,  cmp_r_c_float_s4(i) ))       call zzrc(50+i)

         end do

end program
