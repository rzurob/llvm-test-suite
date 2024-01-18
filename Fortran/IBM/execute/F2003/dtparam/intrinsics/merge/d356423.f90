!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356423.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 19 2008 
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
!* 1. DEFECT 356423 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   contains

elemental function getChar(ch)
    character(*),intent(in) :: ch
    character(3) :: getChar 

    getChar=ch
end function
end module

program d356423
   use m
   implicit none

   character(:),allocatable :: c1(:)
   c1=getChar(["abc","def"])
   
   if(size(c1,1) /=2)                              error stop 10_4
   if(any(c1 /= ["abc","def"]))                    error stop 11_4 

end program

