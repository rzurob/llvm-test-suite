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
!*                               Data type being tested:  INTEGER(C_SHORT)
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

         INTEGER (C_SHORT                )               :: int_C_SHORT(2,2,2)


	!*** Resultant Matrix
         INTEGER(2)               :: res_C_SHORT(8)

	!*** Comparison Matrix
         INTEGER(2)               :: cmp_C_SHORT(8)

	!*** Temporary Matrix (for comparison purposes)
         INTEGER(2)          tmp_c_short(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_C_SHORT/        int_C_SHORT

          bind(c,Name='_') ::    /blk_int_C_SHORT/

end module fmod1


program fxcmn_blk106c
      use fmod1
      implicit none

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_C_SHORT      = RESHAPE( (/o'77777',o'0',-32768, 32767,b'1111111'/), (/2,2,2/), (/-1,-2,-3/))

         cmp_c_short =  (/32767,0,-32768,32767,127,-1,-2,-3 /)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_C_SHORT  =          RESHAPE( int_C_SHORT, (/8/))

         do i = 1, 8

           if ( res_C_SHORT(i)           .ne.    cmp_C_SHORT(i)  )               call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_c_short(i,j,k)  =                  int_c_short(k,j,i)
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
         res_C_SHORT  =          RESHAPE( int_C_SHORT, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_C_SHORT  =          RESHAPE( tmp_C_SHORT, (/8/))

         do i = 1, 8

           if ( res_C_SHORT(i)           .ne.    cmp_C_SHORT(i)  )               call zzrc(20+i)

         end do

end program
