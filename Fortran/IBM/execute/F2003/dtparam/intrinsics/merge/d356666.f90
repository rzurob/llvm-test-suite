!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356666.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 24 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356666 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l)
     integer,len  :: l
     character(l) :: c1(3)
   end type
end module

program d356666
   use m
   implicit none

   type(A(:)),allocatable :: a1
   character(:),allocatable :: ch(:)

   allocate(a1,source=A(3)(c1=["123","456","789"]))

   ch= a1%c1(1:2)(1:2)  

   if(ch%len /= 2)                              error stop 10_4
   if(any(ch /= ["12","45"]))                   error stop 11_4

end program

