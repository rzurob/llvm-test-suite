!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk126a cxcmn_blk126
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk126a fxcmn_blk126a.out
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
!*                               Data type being tested:  real*8
!*					
!*                               Test: BIND(C) common block in internal subroutine 
!*					
!* ===================================================================
!*  REVISION HISTORY					
!*  MM/DD/YY:  Init:  Comments:			
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk126a
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i, j, k
        integer*4,parameter     :: N=2


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         real (kind=INT((4.4e0_8,6.5e0_8))+4 )   :: real_s8(N,N,N)               !* real*8 


       !*** Resultant Matrix 
         real(8)         :: res_real_s8(8)  

       !*** Comparison and Temporary Matrix 
         real(8)         :: cmp_real_s8(8) , tmp_real_s8(N,N,N) 

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label  
! ----------------------------------------------------------------------------

         common /blk_real_s8/          real_s8 

         bind(c, Name='_1') ::   /blk_real_s8/ 

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         real_s8         =  -2.225073D-308 

         cmp_real_s8          = (/-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308/) 


! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison 
         res_real_s8          = RESHAPE(real_s8, (/8/)) 

         do i = 1, 8   

            if ( .not. precision_r8( res_real_s8(i)      ,  cmp_real_s8(i) ))            call zzrc(10+i) 

         end do     


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = -2, -1 
            do j = -2, -1 
               do i = -2, -1 
                 tmp_real_s8(i+3,j+3,k+3)        = real_s8(k+3,j+3,i+3) 
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

         cmp_real_s8             = RESHAPE( tmp_real_s8, (/8/)) 

         res_real_s8          = RESHAPE(real_s8, (/8/)) 

         do i = 1, 8   

            if ( .not. precision_r4( res_real_s8(i)      ,  cmp_real_s8(i) ))            call zzrc(20+i) 

         end do    

   end subroutine

end program
