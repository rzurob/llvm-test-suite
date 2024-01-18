!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk118d cxcmn_blk118
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk118d fxcmn_blk118d.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : March 19, 2004
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
!*				 variables inside of common blocks are interoperable 
!*				 with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  INTEGER(C_INT_LEAST16_T)
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
    INTEGER(2)               :: res_C_INT_LEAST16_T(8)

   !*** Comparison Matrix
    INTEGER(2)               :: cmp_C_INT_LEAST16_T(8)

   !*** Temporary Matrix (for comparison purposes)
    INTEGER(2)         tmp_c_int_least16_t(2,2,2)


   CONTAINS
     subroutine Intern_FSub()
	use iso_c_binding
        implicit none

        integer i, j, k

! ----------------------------------------------------------------------------
! Integer Declaration
! ----------------------------------------------------------------------------

         INTEGER (C_INT_LEAST16_T        )               :: int_C_INT_LEAST16_T(2,2,2) 


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label  
! ----------------------------------------------------------------------------

          common /blk_int_C_INT_LEAST16_T/ int_C_INT_LEAST16_T 

          bind(c,Name='_') ::    /blk_int_C_INT_LEAST16_T/ 

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

         int_C_INT_LEAST16_T     = RESHAPE( (/o'77777',o'0',-32768, 32767,b'1111111'/), (/2,2,2/), (/-1,-2,-3/)) 

         cmp_c_int_least16_t=  (/32767,0,-32768,32767,127,-1,-2,-3 /)        

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         !*** Reshape 3-disensional array to 1-dimensional array for easier comparison 
         res_C_INT_LEAST16_T  =  RESHAPE( int_C_INT_LEAST16_T, (/8/)) 

         do i = 1, 8   

           if ( res_C_INT_LEAST16_T(i)   .ne.    cmp_C_INT_LEAST16_T(i))         call zzrc(10+i) 

         end do     


! ----------------------------------------------------------------------------
! Temporary Matrix Initialization
! - switch first dimension with the third for comparison purposes
! ----------------------------------------------------------------------------

         do k = 1, 2 
            do j = 1, 2 
               do i = 1, 2 
                 tmp_c_int_least16_t(i,j,k)  =          int_c_int_least16_t(k,j,i) 
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
         res_C_INT_LEAST16_T  =  RESHAPE( int_C_INT_LEAST16_T, (/8/)) 

         !*** Reshape 3-dimensional temporary array into 1-dimensional comparison array 
         cmp_C_INT_LEAST16_T  =  RESHAPE( tmp_C_INT_LEAST16_T, (/8/)) 

         do i = 1, 8   

           if ( res_C_INT_LEAST16_T(i)   .ne.    cmp_C_INT_LEAST16_T(i))         call zzrc(20+i) 

         end do    

     end subroutine

end module fmod1 



program fxcmn_blk118d
	use fmod1 
	use iso_c_binding
        implicit none

        integer i, j, k

! ----------------------------------------------------------------------------
! Integer Declaration
! ----------------------------------------------------------------------------

         INTEGER (C_INT_LEAST16_T        )               :: int_C_INT_LEAST16_T(2,2,2) 


! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label  
! ----------------------------------------------------------------------------

          common /blk_int_C_INT_LEAST16_T/ int_C_INT_LEAST16_T 

          bind(c,Name='_') ::    /blk_int_C_INT_LEAST16_T/ 

	!*** Call module subroutine
	call Intern_FSub()

! ---------------------------------------------------------------------------- 
! Integer Verification 
! - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

         do i = 1, 8   

           if ( res_C_INT_LEAST16_T(i)   .ne.    cmp_C_INT_LEAST16_T(i))         call zzrc(50+i) 

         end do    

end program
