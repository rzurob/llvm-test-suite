! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : module_subprogram24f.f
!*
!* PROGRAMMER                   : Bernard Kan
!* DATE                         : 28 May 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008 submodule
!* SECONDARY FUNTIONS TESTED    : view a module procedure in a debugger
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!* based on debug/func/dummyarg21
!*
!* Test that the dummy arguments and function result of the separate
!* module subprogram are visible in the debugger with -g
!*
!* Note that this test will be restricted to verifying the visibility
!* of the specified arguments, since the debugger output values are
!* inconsistent even without submodule.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
  ! Level 3 derived type
  type dtl3
    integer, allocatable :: i
    byte, allocatable :: b
    real :: r
  endtype dtl3

  ! Level 2 derived type
  type dtl2
    integer :: i
    type(dtl3) :: l3
    type(dtl3), allocatable :: l3alloc
  endtype dtl2

  ! These level 2 derived types are pointed to by "ptr" in the level 1 type
  type(dtl2), allocatable, target :: l2In,l2Out

  ! Prints the contents of the derived types
  interface display
    module subroutine displayl2(x)
      type(dtl2), intent(in) :: x
    end subroutine
    module subroutine displayl3(x)
      type(dtl3), intent(in) :: x
    end subroutine
  end interface
end module mod

submodule (mod) subMod1
contains
  module procedure displayl2
   
    print *, "    i",x%i
  end

  module procedure displayl3
    type(dtl3), intent(in) :: x
 
    print *, "      r", x%r
  end
end submodule

program main
  use mod
  integer :: status

  ! Allocate the level 2's that are targets of pointers
  status = -1
  allocate(l2In, l2Out, stat=status)
  print *, "status", status
  l2In = dtl2(-2,dtl3(5,3,2.0),dtl3(2,2,2.0))
  l2Out = dtl2(-4,dtl3(null(),null(),3.0),null())
  call display(l2In)
end program
