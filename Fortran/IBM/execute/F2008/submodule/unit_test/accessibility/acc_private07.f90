module anc_mod
  implicit none

  type base
    integer i1
  end type

  integer, parameter :: var1 = 5
  integer, parameter :: var2 = 7 

  type(base), pointer :: b1 
  type(base), target :: tar
  private b1
  private tar 

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
    if (associated(b1)) then
      b1%i1 =  var1 
    else
      allocate(b1)
      b1%i1 = var1 
    end if
  end subroutine 
end submodule

submodule (anc_mod:submod1) submod2
implicit none
contains
  module subroutine mod_sub()
    b1 => tar
    call s1()
    call print_mod_var()
    print*, b1 
    call s2()
    call print_mod_var()
    print*, b1 
  end subroutine 

  subroutine s2()
    if (associated(b1)) then
      nullify(b1)
      allocate(b1)
      b1%i1 = var2 
    else
      allocate(b1)
      b1%i1 = var2 
    end if
  end 
end submodule

program main
  use anc_mod
  implicit none
  call mod_sub()
end
