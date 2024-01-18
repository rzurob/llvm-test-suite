module anc_mod
  implicit none

  type, private :: base
    integer i1
  end type

  interface
    module subroutine mod_sub()
    end subroutine 
  end interface
contains
  subroutine print_mod_var(arg)
    type(base) :: arg
    print*, arg
  end subroutine
end module

submodule (anc_mod) submod1
implicit none
contains
  subroutine s1(arg)
    type(base) :: arg
    arg%i1 = 5 
  end subroutine 
end submodule

submodule (anc_mod:submod1) submod2
implicit none
contains
  module subroutine mod_sub()
    type(base) :: b1
    call s1(b1)
    call print_mod_var(b1)
    print*, b1 
    call s2(b1)
    call print_mod_var(b1)
    print*, b1 
  end subroutine 

  subroutine s2(arg)
    type(base) :: arg 
    arg%i1 = 7
  end 
end submodule

program main
  use anc_mod
  implicit none
  call mod_sub()
end
