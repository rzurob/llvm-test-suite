!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyBasic04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 18 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. polymorphic allocatable derived type is array 
!*  2. parent type and child type both have allocatable derived type component with deferred length parameter
!*  3. pass allocatable component in last procedure, and modify component value, and verify result of actual argument  
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type nest(l)
     integer,len  :: l
     character(l),allocatable :: c
  end type 
  type base(l1)
      integer,len  :: l1
      type(nest(:)),allocatable :: nest1
  end type

  type,extends(base) :: child(l2)
      integer,len  :: l2
      type(nest(:)),allocatable :: nest2
  end type

  contains

    subroutine sub1(arg)

      class(base(:)),allocatable, intent(inout) :: arg(:) 
      select type(arg)
         type is(child(*,*))
            call sub2(arg)
         class default
            error stop 50_4
      end select
    end subroutine

    subroutine sub2(arg)
      type(child(*,*)),intent(inout) :: arg(5:)

      call sub3(arg(5)%nest1,arg(5)%nest2,1)
      call sub3(arg(6)%nest1,arg(6)%nest2,2)
       
    end subroutine

    subroutine sub3(nest1,nest2,flag)

      type(nest(*)),intent(inout) :: nest1 
      type(nest(*)),intent(inout) :: nest2 
      integer,intent(in) :: flag 

      if(flag .eq. 1) then 
         if(nest1%c /= "abcdefg")                   error stop 10_4
         if(nest2%c /= "aabbccddeeffg")             error stop 11_4
         nest1%c = "xyz"
         nest2%c = "xxxyyyzzz" 
      else if(flag .eq. 2) then
         if(nest1%c /= "1234567")                   error stop 12_4
         if(nest2%c /= "1122334455667")             error stop 13_4
         nest1%c = "789"
         nest2%c = "777888999"
      end if 
    end subroutine

end module

program dummyArgDeferPolyBasic04
  use m
  implicit none

  class(base(:)),allocatable :: base1(:)

  allocate(child(3,4) :: base1(0:1))

  allocate(base1(0)%nest1,source=nest(2*base1%l1+1)("abcdefg") )
  allocate(base1(1)%nest1,source=nest(2*base1%l1+1)("1234567") )
  select type(base1)
     type is(child(*,*))
        allocate(base1(0)%nest2,source=nest(3*base1%l2+1)("aabbccddeeffg") )
        allocate(base1(1)%nest2,source=nest(3*base1%l2+1)("1122334455667") )
     class default
        error stop 51_4
  end select

  call sub1(base1)

  select type(base1)
     type is(child(*,*))
       if(base1(0)%nest1%c /= "xyz")                 error stop 14_4
       if(base1(0)%nest2%c /= "xxxyyyzzz")           error stop 15_4
       if(base1(1)%nest1%c /= "789")                 error stop 16_4
       if(base1(1)%nest2%c /= "777888999")           error stop 17_4
     class default
       error stop 52_4
  end select

end program
