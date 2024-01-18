!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d360831.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 9 2008 
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
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner
       sequence
       integer  :: i
   end type
  type outer
     sequence 
     type(inner) :: inn1
  end type

  contains
    subroutine writeDT(dt) 
     type(outer),intent(in) :: dt

     print *, modFun(dt) 
   end subroutine

   function modFun(dt) 
     type(outer),intent(in) :: dt
     type(outer) :: modFun
     modFun=dt
   end function
end module

program d360831
  use m
  implicit none

  type(outer) :: out(1:1)=[outer(inner(99))]

  print *,"|",out%inn1%i,"|" 
  print *,"|",out,"|"

end program
