! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/diag/genop23d.f
! opt variations: -ql

! Generic operator where the dummy arg is a pointer.  We should get an
! error if there is another interface that takes a non-pointer of same type.

module m
  type base(k1)    ! (4)
    integer, kind :: k1
    integer(k1)   :: x
  contains
    generic :: operator(*) => mul
    procedure :: mul => mulbase
  end type

  interface operator(*)
    module procedure mulnonpointer
  end interface
contains
  integer function mulbase ( a, b )
    class(base(4)), intent(in) :: a, b
    pointer :: b

    mulbase = a%x * b%x
  end function

  integer function mulnonpointer ( a, b )
    class(base(4)), intent(in) :: a, b

    mulnonpointer = a%x * b%x * 3
  end function
end module

use m
end
