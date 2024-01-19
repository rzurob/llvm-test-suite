! Generic operator where the dummy arg is a pointer.  We should get an
! error if there is another interface that takes a non-pointer of same type.

module m
  type base
    integer :: x
  contains
    generic :: operator(*) => mul
    procedure :: mul => mulbase
  end type

  interface operator(*)
    module procedure mulnonpointer
  end interface
contains
  integer function mulbase ( a, b )
    class(base), intent(in) :: a, b
    pointer :: b

    mulbase = a%x * b%x
  end function

  integer function mulnonpointer ( a, b )
    class(base), intent(in) :: a, b

    mulnonpointer = a%x * b%x * 3
  end function
end module

use m
end
