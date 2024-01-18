!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357086.f   
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
!*  1. DEFECT 357086
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      character(l1),pointer :: ch1=>null()   
      character(l1),allocatable :: ch2     
   end type
   type,extends(base) :: child
      class(*),pointer :: baseComp=>null()
   end type
end module

program d357086
  use m
  implicit none

  class(base(3)),target,allocatable :: from1

  allocate(child(3) :: from1)
  allocate(from1%ch1,source="xlf")
  from1%ch2="123"

  if(.not. allocated(from1))     stop 1
  select type(from1)
     type is(child(*))
        from1%baseComp=>from1
        if(from1%ch2 /= "123")      stop 2   
        if(.not. associated(from1%baseComp,from1))   stop 3
     class default
         stop 4 
  end select

end program 

