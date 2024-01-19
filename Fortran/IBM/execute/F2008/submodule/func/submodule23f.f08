! *********************************************************************
!* ===================================================================
!*
!* DATE                         : 28 May 2013
!*
!* PRIMARY FUNCTIONS TESTED     : F2008 submodule
!* SECONDARY FUNTIONS TESTED    : bind(c) in a module function or subroutine
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!* based on cintrop_ts29113/optional/func/fcintrpopt040.f
!*
!* Calling a BIND(C) module function or subroutine from C that is
!*   defined in Fortran in a submodule
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  use iso_c_binding
  interface
    module subroutine sub_test1(size, buffer) bind(c)
      use iso_c_binding
      implicit none
      integer(c_int) :: size
      real(c_float), dimension( * ), optional :: buffer
    end subroutine

    integer(c_int) module function func_test1() bind(c)
      use iso_c_binding
      implicit none
    end function
  end interface
end module m

submodule (m) n
integer actual_size
contains
  module subroutine sub_test1(size, buffer) bind(c)
    use iso_c_binding
    implicit none

    logical, external :: precision_r4

    integer(c_int) :: size
    real(c_float), dimension( * ), optional :: buffer
    integer    :: i

    actual_size = 0

    if (present(buffer)) then
        do i = 1, size
           if(precision_r4(buffer(i), i*1.0_4)) then
		actual_size = actual_size + 1
           end if
        end do
    end if

    print *, actual_size
  end

  integer(c_int) module function func_test1() bind(c)
    use iso_c_binding
    implicit none
    func_test1 = actual_size
  end
end submodule n

program p
  use iso_c_binding
  interface
    subroutine c_helper() bind(c)
      import
    end
  end interface
  call c_helper()
end program
