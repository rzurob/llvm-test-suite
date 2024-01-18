!*********************************************************************
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
!*                               Data type being tested:  integer*4
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

         integer                                         :: int_s4(2,2,2)


	!*** Resultant Matrix
         integer                 :: res_s4(8)

	!*** Comparison Matrix
         integer                 :: cmp_s4(8)

	!*** Temporary Matrix (for comparison purposes)
         integer             tmp_s4(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_s4/            int_s4

          bind(c,Name='_') ::    /blk_int_s4/

end module fmod1


program fxcmn_blk103c
      use fmod1
      implicit none

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_s4 = RESHAPE( (/2147483647,b'1111111',-2147483648, 0, o'3641100',-2147483648,2147483647,-0, 1/), (/2,2,2/), (/-1,-2,-3/))

         cmp_s4 =  (/2147483647,127,-2147483648,0,1000000,-2147483648,2147483647,0 /)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_s4  =               RESHAPE( int_s4, (/8/))

         do i = 1, 8

           if ( res_s4(i)                .ne.    cmp_s4(i)   )                   call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_s4(i,j,k)  =                       int_s4(k,j,i)
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
         res_s4  =               RESHAPE( int_s4, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_s4  =               RESHAPE( tmp_s4, (/8/))

         do i = 1, 8

           if ( res_s4(i)                .ne.    cmp_s4(i)   )                   call zzrc(20+i)

         end do

end program
