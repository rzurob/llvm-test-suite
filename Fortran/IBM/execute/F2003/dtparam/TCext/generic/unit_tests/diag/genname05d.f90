! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/unit_tests/diag/genname05d.f
! opt variations: -qnok -qnol

module m
  type dt(k1,n1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: n1
  contains
    procedure :: foo
    generic :: gen => foo, bar  ! Error: foo is function, but bar is a sub
    procedure :: bar
  end type
contains
  integer function foo(a)
    class(dt(4,*)) a
    foo = 3
  end function

  subroutine bar(a)
    class(dt(4,*)) a
  end subroutine
end module
