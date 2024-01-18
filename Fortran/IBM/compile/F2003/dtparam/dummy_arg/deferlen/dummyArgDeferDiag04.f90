!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferDiag04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 5 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Dummy Argument with deferred length 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. If a dummy argument is allocatable or pointer, the associated actual argument shall be polymorphic if and only if the dummy argument is polymorphic and the declared type of the actual arguments shall be same as the declared type of the dummy argument.
!*  2. following is the illegal cases:
!*   1)dummy argument is not polymorphic, but actual argument is polymorphic.
!*   2)dummy argument is polymorphic, but actual argument is not.
!*   3)dummy argument and actual argument are both polymorphic, but declared type is not same. 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
    integer,kind :: k1=2
    integer,len  :: l1=3
  end type
  type,extends(base) :: child(k2,l2)
    integer,kind :: k2=4
    integer,len  :: l2=2
  end type
  contains
    subroutine sub1(arg)
       type(base(2,:)),allocatable :: arg
    end subroutine
    subroutine sub2(arg)
       type(base(2,:)),pointer :: arg
    end subroutine   
end module

program dummyArgDeferDiag04

  use m
  implicit none

  interface

  subroutine sub3(arg)
     import 
     class(base(2,:)),allocatable :: arg
  end subroutine

  subroutine sub4(arg)
     import 
     class(base(2,:)),pointer :: arg
  end subroutine

  end interface 

  class(base(2,:)),allocatable :: base1
  class(base(2,:)),pointer     :: base2=>null()

  type(base(2,:)),allocatable  :: base3
  type(base(2,:)),pointer      :: base4=>null()

  class(base(4,:)),allocatable :: base5
  class(base(4,:)),pointer     :: base6=>null()

  type(child(2,:,4,:)),allocatable :: child1
  type(child(2,:,4,:)),pointer     :: child2=>null()

  call sub1(base1)  
  call sub2(base2)
  call sub3(base3)
  call sub4(base4)
  call sub5(child1)
  call sub6(child2)
  call sub5(base1)
  call sub6(base2)
  call sub3(child1)
  call sub4(child2)
  call sub3(base5)
  call sub4(base6)
  
  contains
     subroutine sub5(arg)
         class(child(2,:,4,:)),allocatable :: arg
     end subroutine 

     subroutine sub6(arg)
         class(child(2,:,4,:)),pointer :: arg
     end subroutine
 
end program

subroutine sub3(arg)
  use m
  class(base(2,:)),allocatable :: arg
end subroutine

subroutine sub4(arg)
  use m
  class(base(2,:)),pointer :: arg
end subroutine
