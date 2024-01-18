!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk130 cxcmn_blk130
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk130 fxcmn_blk130.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : February 13, 2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95, xlc, gcc
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This test case will verify that 3-dimensional array
!*                               variables inside of common blocks are interoperable
!*                               with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  REAL(C_LONG_DOUBLE)
!*
!*                               Scope being tested:  main program
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk130
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i, j, k
        integer*4,parameter     :: N=2


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         REAL (c_LONG_double)                    :: r_c_LONG_double_s8(-2:-1,-2:-1,-2:-1) 


       !*** Resultant Matrix 
         REAL(16)         :: res_r_c_long_double_s8(8) 

       !*** Comparison and Temporary Matrix 
         REAL(16)         :: cmp_r_c_long_double_s8(8), tmp_r_c_long_double_s8(-2:-1,-2:-1,-2:-1) 

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label  
! ----------------------------------------------------------------------------

         common /blk_r_C_LONG_DOUBLE_s8/          r_C_LONG_DOUBLE_s8 

         bind(c, Name='_1') ::   /blk_r_C_LONG_DOUBLE_s8/  

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         r_c_long_double_s8   =  RESHAPE( (/1.797693Q+308, -2.004168Q-292, 0.0Q0, 2.004168Q-292, -1.797693Q+308/), (/2,2,2/), (/1.00000Q+308, -1.00000Q+308, -1.00000Q+100/)) 

         cmp_r_c_long_double_s8    = (/1.797693Q+308, -2.004168Q-292, 0.0Q0, 2.004168Q-292, -1.797693Q+308, 1.00000Q+308, -1.00000Q+308, -1.00000Q+100/) 


! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison 
         res_r_c_long_double_s8    = RESHAPE(r_c_long_double_s8,(/8/)) 

         do i = 1, 8   

            if ( .not. precision_r16( res_r_c_long_double_s8(i),  cmp_r_c_long_double_s8(i) ))      call zzrc(10+i) 

         end do     


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = -2, -1 
            do j = -2, -1 
               do i = -2, -1 
                 tmp_r_c_long_double_s8(i,j,k)   = r_c_long_double_s8(k,j,i) 
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

         cmp_r_c_long_double_s8       = RESHAPE( tmp_r_c_long_double_s8, (/8/)) 

         res_r_c_long_double_s8    = RESHAPE(r_c_long_double_s8,(/8/)) 

         do i = 1, 8   

            if ( .not. precision_r16( res_r_c_long_double_s8(i),  cmp_r_c_long_double_s8(i) ))      call zzrc(20+i) 

         end do    

end program
