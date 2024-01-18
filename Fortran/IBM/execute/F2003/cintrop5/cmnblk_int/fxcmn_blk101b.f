!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk101b cxcmn_blk101
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk101b fxcmn_blk101b.out
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
!*                               Data type being tested:  integer*1
!*
!*                               Test: BIND(C) common block in external subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk101b
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

         integer (LEN('k'))                              :: int_s1(2,2,2)


	!*** Resultant Matrix
         integer(1)              :: res_s1(8)

	!*** Comparison Matrix
         integer(1)              :: cmp_s1(8)

	!*** Temporary Matrix (for comparison purposes)
         integer(1)          tmp_s1(2,2,2)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

          common /blk_int_s1/            int_s1

          bind(c,Name='_') ::    /blk_int_s1/

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_s1      =  RESHAPE( (/b'1111111',o'0',-128,o'177',127,0,-0,-111/), (/2,2,2/))

         cmp_s1 =  (/127,0,-128,127,127,0,0,-111/)

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison
         res_s1  =               RESHAPE( int_s1, (/8/))

         do i = 1, 8

           if ( res_s1(i)                .ne.    cmp_s1(i)   )                   call zzrc(10+i)

         end do


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2
            do j = 1, 2
               do i = 1, 2
                 tmp_s1(i,j,k)  =                       int_s1(k,j,i)
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
         res_s1  =               RESHAPE( int_s1, (/8/))

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array
         cmp_s1  =               RESHAPE( tmp_s1, (/8/))

         do i = 1, 8

           if ( res_s1(i)                .ne.    cmp_s1(i)   )                   call zzrc(20+i)

         end do

   end subroutine
