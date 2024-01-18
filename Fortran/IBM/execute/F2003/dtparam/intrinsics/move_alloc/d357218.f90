!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357218.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 7 2008 
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
!*  1. DEFECT 357218
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
   end type
   type, extends(base) :: child(l2)
      integer,len :: l2
      type(base(l2)) :: b1
   end type
end module

program d357218
  use m
  implicit none

  type(child(1,2)),allocatable :: from1
  class(base(1)),allocatable   :: to1
  type(base(2)),allocatable    :: base1

  allocate(base(2) :: base1)
  base1=base(2)()

  allocate(from1,source=child(1,2)(b1=base1) )

  print *,from1%b1%l1
  call move_alloc(from1,to1)
  select type(to1)
     type is(child(*,*))
     print *,to1%b1%l1
     class default
        stop 1
  end select
end program

