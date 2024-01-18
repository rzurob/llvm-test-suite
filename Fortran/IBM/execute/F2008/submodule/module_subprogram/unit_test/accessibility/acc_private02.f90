module anc_mod
  implicit none

  integer i1
  integer j1
  private i1

  interface
    module subroutine mod_sub()
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
  subroutine s1()
    i1 = 5 
    j1 = 6 
  end subroutine 
end submodule

submodule (anc_mod:submod1) submod2
implicit none
contains
  module procedure mod_sub
    call s1()
    call print_mod_var()
    print*, i1, j1
    call s2()
    call print_mod_var()
    print*, i1, j1
  end

  subroutine s2()
    i1 = 7
    j1 = 8
  end 
end submodule

program main
  use anc_mod
  implicit none
  call mod_sub()
end
