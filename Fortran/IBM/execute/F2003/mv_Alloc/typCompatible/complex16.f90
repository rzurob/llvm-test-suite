! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : complex16.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : TO is of complex*16, dummy arg
!*                               FROM is of complex(8) with private attr
!*                               rank 2
!*                               
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
       complex(8), private :: c1 (:,:)
       allocatable c1

       contains 
       subroutine sub(a)
           complex*16, allocatable :: a(:,:) 

           allocate(c1(1,1)) 
          
           c1 = cmplx(3.0,1.0, 8)

           call move_alloc(c1, a)
       
           if ( allocated(c1) ) stop 11
       end subroutine
end module


program main 

       use m
       complex*16 :: c2(:,:)

       allocatable c2
       logical precision_x6

       allocate(c2(2:0, 0:1))

       call sub(c2)

       if ( size(c2,1) /= 1) stop 21 
       if ( size(c2,2) /= 1) stop 23

       if ( .not. allocated(c2) ) stop 31 
       write ( 6, 100) c2(1,1)
100 format ( "(", 2f20.17, ")")

     end
