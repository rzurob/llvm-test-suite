module m1
  implicit none
  type other
    integer, allocatable ::  m(:)
    integer, allocatable ::  n(:)
  end type
  type(other), allocatable :: o1
  private other
end module

module anc_mod
  use m1
  implicit none

  type base
    private
    integer, allocatable :: i1(:)
    contains 
      procedure, nopass, private :: printme => print_mod_var
  end type

  type(base), pointer :: b1 
  type(base), target :: tar

  private b1
  private tar 
  private print_mod_var

  interface
    module subroutine mod_sub()
    end subroutine 
  end interface
contains
  subroutine print_mod_var()
    print*, b1%i1 
  end subroutine
  subroutine init()
    allocate(o1)
    allocate(o1%m(5))
    o1%m = 5
    allocate(o1%n(7))
    o1%n = 7 
  end
end module

submodule (anc_mod) submod1
implicit none
contains
  subroutine s1()
    if (associated(b1)) then
      b1%i1 = o1%m 
    else
      allocate(b1)
      b1%i1 = o1%m 
    end if
  end subroutine 
end submodule

submodule (anc_mod:submod1) submod2
implicit none
contains
  module procedure mod_sub
    type(other) :: o2
    call init()
    b1 => tar
    call s1()
    call print_mod_var()
    call b1%printme()
    print*, b1%i1 
    call s2()
    call print_mod_var()
    call b1%printme()
    print*, b1%i1 
  end 

  subroutine s2()
    if (associated(b1)) then
      nullify(b1)
      allocate(b1)
      b1%i1 = o1%n 
    else
      allocate(b1)
      b1%i1 = o1%n 
    end if
  end 
end submodule

program main
  use anc_mod
  implicit none
  call mod_sub()
end
