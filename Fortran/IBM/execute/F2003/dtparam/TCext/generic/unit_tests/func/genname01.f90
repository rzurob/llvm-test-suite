! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/func/genname01.f
! opt variations: -qnol

! Generic names with optional args, and arg keywords

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure :: sadd1
    generic :: gadd => sadd1, sadd2, sadd3
    procedure :: sadd2
    procedure, pass(a) :: sadd3
  end type
contains
  integer function sadd1(a)
    class(dt(*,4)) :: a
    sadd1 = a%i
  end function

  integer function sadd2(a,b)
    class(dt(*,4)) :: a
    integer :: b
    sadd2 = a%i + b
  end function

  integer function sadd3(c,b,a)
    integer, optional :: c
    character(*) :: b
    class(dt(*,4)) :: a
    sadd3 = a%i + len(b) + 1
    if (present(c)) then
      sadd3 = sadd3 + c
    endif
  end function

  subroutine sub
    type(dt(20,4)) :: x = dt(20,4)(3)
    integer i
    i = x%gadd()
    if (i /= 3) then
      print *, i
      error stop 1_4
    endif
    i = x%gadd(3)
    if (i /= 6) then
      print *, i
      error stop 2_4
    endif
    i = x%gadd(b=-3)
    if (i /= 0) then
      print *, i
      error stop 3_4
    endif
    i = x%gadd(1,'ab')
    if (i /= 7) then
      print *, i
      error stop 4_4
    endif
    i = x%gadd(b='ab')
    if (i /= 6) then
      print *, i
      error stop 5_4
    endif
    i = x%gadd(1,b='abc')
    if (i /= 8) then
      print *, i
      error stop 6_4
    endif
  end subroutine
end module

use m
call sub
end

