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
!*                               Data type being tested:  integer*2
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
    integer(2)              :: res_s2(8)

   !*** Comparison Matrix
    integer(2)              :: cmp_s2(8)

   !*** Temporary Matrix (for comparison purposes)
    integer(2)          tmp_s2(2,2,2)


   CONTAINS
     subroutine Intern_FSub()
	use iso_c_binding
        implicit none

        integer i, j, k

! ----------------------------------------------------------------------------
! Integer Declaration
! ----------------------------------------------------------------------------

         integer (2 )                                    :: int_s2(2,2,2)


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_s2/            int_s2

          bind(c,Name='_') ::    /blk_int_s2/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_s2      =  RESHAPE( (/o'77777',o'0',-32768, 32767,b'1111111',0,-1277/), (/2,2,2/), (/-1,-2,-3/))

         cmp_s2 =  (/32767,0,-32768,32767,127,0,-1277,-1 /)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_s2  =               RESHAPE( int_s2, (/8/))

         do i = 1, 8

           if ( res_s2(i)                .ne.    cmp_s2(i)   )                   call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_s2(i,j,k)  =                       int_s2(k,j,i)
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
         res_s2  =               RESHAPE( int_s2, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_s2  =               RESHAPE( tmp_s2, (/8/))

         do i = 1, 8

           if ( res_s2(i)                .ne.    cmp_s2(i)   )                   call zzrc(20+i)

         end do

     end subroutine

end module fmod1



program fxcmn_blk102d
	use fmod1
	use iso_c_binding
        implicit none

        integer i, j, k

! ----------------------------------------------------------------------------
! Integer Declaration
! ----------------------------------------------------------------------------

         integer (2 )                                    :: int_s2(2,2,2)


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_s2/            int_s2

          bind(c,Name='_') ::    /blk_int_s2/

	!*** Call module subroutine
	call Intern_FSub()

! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

         do i = 1, 8

           if ( res_s2(i)                .ne.    cmp_s2(i)   )                   call zzrc(50+i)

         end do

end program