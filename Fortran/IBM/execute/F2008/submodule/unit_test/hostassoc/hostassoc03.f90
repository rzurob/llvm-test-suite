module other_mod
implicit none

  type base
    integer :: i1  = 1
  end type

  integer ::  ii1 = 1
  real :: rr1 = 1.0
  character(:), allocatable :: cc1

  type(base), save :: b1 
contains
  subroutine print_other_mod_var()
    print*, ii1 
    print*, rr1 
    if (allocated(cc1)) then 
      print*, cc1 
    end if
    print*, b1 
  end subroutine
end module

module anc_mod
  implicit none

  type base
    integer :: i1  = 0
  end type

  integer ::  ii1 = 0
  real :: rr1 = 0.0
  character(:), allocatable :: cc1

  type(base), save :: b1 
  private b1

  interface
    module subroutine mod_sub()
    end subroutine 
  end interface
contains
  subroutine print_mod_var()
    print*, ii1 
    print*, rr1 
    if (allocated(cc1)) then 
      print*, cc1 
    end if
    print*, b1 
  end subroutine
end module

submodule (anc_mod) submod1
use other_mod, only : ii1
implicit none
contains
  subroutine s1()
    ii1 = 5 
    rr1 = 5.0 
    cc1 = "HELLO5"
    b1%i1 = 5 
  end subroutine 
end submodule

submodule (anc_mod:submod1) submod2
use other_mod, only : rr1
implicit none
contains
  subroutine s2()
    ii1 = 7 
    rr1 = 7.0 
    cc1 = "HELLO7"
    b1%i1 = 7 
  end 
end submodule

submodule (anc_mod:submod2) submod3
use other_mod, only : cc1, print_other_mod_var
implicit none
contains
  module subroutine mod_sub()
    call s1()
    call print_mod_var()
    call print_other_mod_var()
    print*, ii1 
    print*, rr1 
    if (allocated(cc1)) then 
      print*, cc1 
    end if
    print*, b1 
    call s2()
    call print_mod_var()
    call print_other_mod_var()
    print*, ii1 
    print*, rr1 
    if (allocated(cc1)) then 
      print*, cc1 
    end if
    print*, b1 
    call s3()
    call print_mod_var()
    call print_other_mod_var()
    print*, ii1 
    print*, rr1 
    if (allocated(cc1)) then 
      print*, cc1 
    end if
    print*, b1 
  end subroutine 

  subroutine s3()
    ii1 = 9 
    rr1 = 9.0 
    cc1 = "HELLO9"
    b1%i1 = 9 
  end 
end submodule


program main
  use anc_mod
  implicit none
  call mod_sub()
end
