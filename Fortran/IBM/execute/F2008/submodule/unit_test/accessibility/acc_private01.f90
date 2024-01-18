module anc_mod
  implicit none

  integer i1
  integer j1
  private i1

  interface
    module subroutine s1()
    end subroutine
  end interface

contains
  subroutine print_mod_var()
    print*, i1, j1
  end subroutine
end module

submodule (anc_mod) submod1
implicit none

contains
  module subroutine s1()
    i1 = 5
    j1 = 6
    call print_mod_var()
    print*, i1, j1
  end subroutine
end submodule

program main
  use anc_mod

  implicit none

  call s1()
end
