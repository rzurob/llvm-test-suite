!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk107c cxcmn_blk107
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk107c fxcmn_blk107c.out
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
!*                               Data type being tested:  INTEGER(C_INT)
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

         INTEGER (C_INT                  )               :: int_C_INT(2,2,2)


	!*** Resultant Matrix
         INTEGER(4)               :: res_C_INT(8)

	!*** Comparison Matrix
         INTEGER(4)               :: cmp_C_INT(8)

	!*** Temporary Matrix (for comparison purposes)
         INTEGER(4)          tmp_c_int(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_C_INT/          int_C_INT

          bind(c,Name='_') ::    /blk_int_C_INT/

end module fmod1


program fxcmn_blk107c
      use fmod1
      implicit none

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_C_INT        = RESHAPE( (/2147483647,b'1111111',-2147483648, 0, o'3641100'/), (/2,2,2/), (/-1,-2,-3/))

         cmp_c_int =  (/2147483647,127,-2147483648,0,1000000,-1,-2,-3 /)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_C_INT  =            RESHAPE( int_C_INT, (/8/))

         do i = 1, 8

           if ( res_C_INT(i)             .ne.    cmp_C_INT(i) )                  call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_c_int(i,j,k)  =                    int_c_int(k,j,i)
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
         res_C_INT  =            RESHAPE( int_C_INT, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_C_INT  =            RESHAPE( tmp_C_INT, (/8/))

         do i = 1, 8

           if ( res_C_INT(i)             .ne.    cmp_C_INT(i) )                  call zzrc(20+i)

         end do

end program
