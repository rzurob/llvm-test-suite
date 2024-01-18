!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk125 cxcmn_blk125
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk125 fxcmn_blk125.out
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
!*                               Data type being tested:  real
!*
!*                               Scope being tested:  main program
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk125
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i, j, k
        integer*4,parameter     :: N=2


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         real , DIMENSION(-2:-1,-2:-1,-2:-1)     :: real_s4                      !* real*4 


       !*** Resultant Matrix 
         real            :: res_real_s4(8) 

       !*** Comparison and Temporary Matrix 
         real            :: cmp_real_s4(8), tmp_real_s4(-2:-1,-2:-1,-2:-1) 

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label  
! ----------------------------------------------------------------------------

         common /blk_real_s4/          real_s4 

         bind(c, Name='_1') ::   /blk_real_s4/   

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         real_s4         =  RESHAPE( (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/), (/2,2,2/), (/1.0,-1.0,-2.9/)) 

         cmp_real_s4          = (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38, 1.0, -1.0, -2.9/) 


! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison 
         res_real_s4          = RESHAPE(real_s4, (/8/)) 

         do i = 1, 8   

            if ( .not. precision_r4( res_real_s4(i)      ,  cmp_real_s4(i) ))            call zzrc(10+i) 

         end do     


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = -2, -1 
            do j = -2, -1 
               do i = -2, -1 
                 tmp_real_s4(i,j,k)              = real_s4(k,j,i) 
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

         cmp_real_s4             = RESHAPE( tmp_real_s4, (/8/)) 

         res_real_s4          = RESHAPE(real_s4, (/8/)) 

         do i = 1, 8   

            if ( .not. precision_r4( res_real_s4(i)      ,  cmp_real_s4(i) ))            call zzrc(20+i) 

         end do    

end program
