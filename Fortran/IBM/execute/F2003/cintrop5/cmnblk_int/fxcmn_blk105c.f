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
!*                               Data type being tested:  INTEGER(C_SIGNED_CHAR)
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

         INTEGER (C_SIGNED_CHAR          )               :: int_C_SIGNED_CHAR(2,2,2)


	!*** Resultant Matrix
         INTEGER(1)               :: res_C_SIGNED_CHAR(8)

	!*** Comparison Matrix
         INTEGER(1)               :: cmp_C_SIGNED_CHAR(8)

	!*** Temporary Matrix (for comparison purposes)
         INTEGER(1)          tmp_c_signed_char(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_C_SIGNED_CHAR/  int_C_SIGNED_CHAR

          bind(c,Name='_') ::    /blk_int_C_SIGNED_CHAR/

end module fmod1


program fxcmn_blk105c
      use fmod1
      implicit none

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_C_SIGNED_CHAR       = RESHAPE( (/b'1111111',o'0',-128, o'177',127/), (/2,2,2/), (/-1,-2,-3/))

         cmp_c_signed_char =  (/127,0,-128,127,127,-1,-2,-3 /)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_C_SIGNED_CHAR  =    RESHAPE( int_C_SIGNED_CHAR, (/8/))

         do i = 1, 8

           if ( res_C_SIGNED_CHAR(i)     .ne.    cmp_C_SIGNED_CHAR(i) )          call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_c_signed_char(i,j,k)  =            int_c_signed_char(k,j,i)
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
         res_C_SIGNED_CHAR  =    RESHAPE( int_C_SIGNED_CHAR, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_C_SIGNED_CHAR  =    RESHAPE( tmp_C_SIGNED_CHAR, (/8/))

         do i = 1, 8

           if ( res_C_SIGNED_CHAR(i)     .ne.    cmp_C_SIGNED_CHAR(i) )          call zzrc(20+i)

         end do

end program