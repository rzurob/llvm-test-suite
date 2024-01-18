!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk119c cxcmn_blk119
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk119c fxcmn_blk119c.out
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 19, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that 3-dimensional array
!*				 variables inside of common blocks are interoperable
!*				 with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  INTEGER(C_INT_LEAST32_T)
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

        integer i, j, k

! ----------------------------------------------------------------------------
! Integer Declaration
! ----------------------------------------------------------------------------

         INTEGER (C_INT_LEAST32_T        )               :: int_C_INT_LEAST32_T(2,2,2)


	!*** Resultant Matrix
         INTEGER(4)               :: res_C_INT_LEAST32_T(8)

	!*** Comparison Matrix
         INTEGER(4)               :: cmp_C_INT_LEAST32_T(8)

	!*** Temporary Matrix (for comparison purposes)
         INTEGER(4)         tmp_c_int_least32_t(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_C_INT_LEAST32_T/ int_C_INT_LEAST32_T

          bind(c,Name='_') ::    /blk_int_C_INT_LEAST32_T/

end module fmod1


program fxcmn_blk119c
      use fmod1
      implicit none

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_C_INT_LEAST32_T     =  5

         cmp_c_int_least32_t =  (/5,5,5,5,5,5,5,5 /)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_C_INT_LEAST32_T  =  RESHAPE( int_C_INT_LEAST32_T, (/8/))

         do i = 1, 8

           if ( res_C_INT_LEAST32_T(i)   .ne.    cmp_C_INT_LEAST32_T(i))         call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_c_int_least32_t(i,j,k)  =          int_c_int_least32_t(k,j,i)
             end do
           end do
         end do

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         !*** Reshape 3-dimensional resultant array into 1-dimensional resultant array for easier comparison
         res_C_INT_LEAST32_T  =  RESHAPE( int_C_INT_LEAST32_T, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_C_INT_LEAST32_T  =  RESHAPE( tmp_C_INT_LEAST32_T, (/8/))

         do i = 1, 8

           if ( res_C_INT_LEAST32_T(i)   .ne.    cmp_C_INT_LEAST32_T(i))         call zzrc(20+i)

         end do

end program
