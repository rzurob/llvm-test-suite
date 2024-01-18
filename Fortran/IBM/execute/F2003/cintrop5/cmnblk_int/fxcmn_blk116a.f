!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk116a cxcmn_blk116
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk116a fxcmn_blk116a.out
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
!*                               Data type being tested:  INTEGER(C_INT64_T)
!*
!*                               Test: BIND(C) common block in internal subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk116a
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none

        integer i, j, k

! ----------------------------------------------------------------------------
! Integer Declaration
! ----------------------------------------------------------------------------

         INTEGER (C_INT64_T              )               :: int_C_INT64_T(2,2,2)


	!*** Resultant Matrix
         INTEGER(8)               :: res_C_INT64_T(8)

	!*** Comparison Matrix
         INTEGER(8)               :: cmp_C_INT64_T(8)

	!*** Temporary Matrix (for comparison purposes)
         INTEGER(8)         tmp_c_int64_t(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_C_INT64_T/      int_C_INT64_T

          bind(c,Name='_') ::    /blk_int_C_INT64_T/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_C_INT64_T    =  9223372036850123456_8

         cmp_c_int64_t =  (/9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8/)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_C_INT64_T  =        RESHAPE( int_C_INT64_T, (/8/))

         do i = 1, 8

           if ( res_C_INT64_T(i)         .ne.    cmp_C_INT64_T(i))               call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_c_int64_t(i,j,k)  =                int_c_int64_t(k,j,i)
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
         res_C_INT64_T  =        RESHAPE( int_C_INT64_T, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_C_INT64_T  =        RESHAPE( tmp_C_INT64_T, (/8/))

         do i = 1, 8

           if ( res_C_INT64_T(i)         .ne.    cmp_C_INT64_T(i))               call zzrc(20+i)

         end do

   end subroutine

end program
