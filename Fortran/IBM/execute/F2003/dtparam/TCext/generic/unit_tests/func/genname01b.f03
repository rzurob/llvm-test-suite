! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/func/genname01b.f
! opt variations: -ql

! Generic names

module m
  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure :: sadd1b => sadd1
    generic :: gadd => sadd1b, sadd2b
    procedure :: sadd2b => sadd2
  end type

  interface foo
    module procedure sadd1
    module procedure sadd2
  end interface
contains
  subroutine sadd1(a)
    class(dt(4)) :: a
    a%i = a%i + 1
  end subroutine

  subroutine sadd2(a,b)
    class(dt(4)) :: a
    integer :: b
    a%i = a%i + b
  end subroutine

  subroutine sub
    type(dt(4)) :: x = dt(4)(3)
    integer i
    call x%gadd(3)
    if (x%i /= 6) then
      print *, x%i
      error stop 1_4
    endif
    call x%gadd()
    if (x%i /= 7) then
      print *, x%i
      error stop 2_4
    end if
    call x%gadd(b=-3)
    if (x%i /= 4) then
      print *, x%i
      error stop 3_4
    endif
  end subroutine
end module

use m
call sub
end
