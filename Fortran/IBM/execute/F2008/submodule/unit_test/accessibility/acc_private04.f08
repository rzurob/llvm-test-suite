module anc_mod
  implicit none

  type base
    integer i1
  end type

  type(base) :: b1
  private b1

  interface
    module subroutine mod_sub()
    end subroutine
  end interface
contains
  subroutine print_mod_var()
    print*, b1
  end subroutine
end module

submodule (anc_mod) submod1
implicit none
contains
  subroutine s1()
    b1%i1 = 5
  end subroutine
end submodule

submodule (anc_mod:submod1) submod2
implicit none
contains
  module subroutine mod_sub()
    call s1()
    call print_mod_var()
    print*, b1
    call s2()
    call print_mod_var()
    print*, b1
  end subroutine

  subroutine s2()
    b1%i1 = 7
  end
end submodule

program main
  use anc_mod
  implicit none
  call mod_sub()
end