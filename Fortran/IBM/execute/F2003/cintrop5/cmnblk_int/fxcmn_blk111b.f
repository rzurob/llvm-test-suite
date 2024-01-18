!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk111b cxcmn_blk111
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk111b fxcmn_blk111b.out
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
!*                               Data type being tested:  INTEGER(C_INTPTR_T)
!*
!*                               Test: BIND(C) common block in external subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk111b
	use iso_c_binding
        implicit none

      call extern_fsub()

End program

subroutine extern_fsub()
	use iso_c_binding
        implicit none

        integer i, j, k

! ----------------------------------------------------------------------------
! Integer Declaration
! ----------------------------------------------------------------------------

         INTEGER (C_INTPTR_T             )               :: int_C_INTPTR_T(2,2,2)


	!*** Resultant Matrix
         INTEGER(4)               :: res_C_INTPTR_T(8)

	!*** Comparison Matrix
         INTEGER(4)               :: cmp_C_INTPTR_T(8)

	!*** Temporary Matrix (for comparison purposes)
         INTEGER(4)          tmp_c_intptr_t(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_C_INTPTR_T/     int_C_INTPTR_T

          bind(c,Name='_') ::    /blk_int_C_INTPTR_T/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_C_INTPTR_T   =  1000000000

         cmp_c_intptr_t =  (/1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000 /)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_C_INTPTR_T  =       RESHAPE( int_C_INTPTR_T, (/8/))

         do i = 1, 8

           if ( res_C_INTPTR_T(i)        .ne.    cmp_C_INTPTR_T(i))              call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_c_intptr_t(i,j,k)  =               int_c_intptr_t(k,j,i)
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
         res_C_INTPTR_T  =       RESHAPE( int_C_INTPTR_T, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_C_INTPTR_T  =       RESHAPE( tmp_C_INTPTR_T, (/8/))

         do i = 1, 8

           if ( res_C_INTPTR_T(i)        .ne.    cmp_C_INTPTR_T(i))              call zzrc(20+i)

         end do

   end subroutine
