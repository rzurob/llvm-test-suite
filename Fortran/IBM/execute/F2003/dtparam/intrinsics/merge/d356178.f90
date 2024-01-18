!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356178.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 15 2008 
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
!* 1. DEFECT 356178 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
       integer, len  :: l1
       character(l1) :: ca
   end type
   type B
       type(A(:)),allocatable :: type1
   end type
end module

program d356178
   use m
   implicit none

   type(A(4)) :: a1=A(4)(ca="123")
   type(B)    :: b1=B(type1=null())

   b1%type1=a1

   if (b1%type1%ca /= '123') stop 20
   if (len(b1%type1%ca) /= 4) stop 21

   call associate_replacer (b1%type1%ca)

!   associate(x=>b1%type1%ca)
!      if(x /= "123")                         error stop 10_4
!      if(x%len /= len(x) .or. x%len /= 4)    error stop 11_4
!   end associate

    contains

    subroutine associate_replacer (x)
        character(*), intent(in) :: x

        if(x /= "123")                         error stop 10_4
        if(x%len /= len(x) .or. x%len /= 4)    error stop 11_4
    end subroutine
end program

