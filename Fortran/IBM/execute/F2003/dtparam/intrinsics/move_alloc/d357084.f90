!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357084.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 6 2008 
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
!*  1. DEFECT 357084
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      character(l1),pointer :: ch1=>null()
   end type
   type,extends(base) :: child
   end type
end module

program d357084

  use m
  implicit none

  class(base(3)),pointer :: base1

  allocate(child(3) :: base1)
  allocate(base1%ch1,source="xlf")

  print *,"|",base1%ch1,"|",base1%l1,len(base1%ch1),base1%ch1%len
  select type(base1)
     type is(child(*))
         print *,"|",base1%ch1,"|",base1%l1,len(base1%ch1),base1%ch1%len
     class default
         error stop 100_4
  end select

end program

