module m
  type dt(k,l)
    integer, kind :: k
    integer, len :: l
    integer(k) i(l)
  end type

  type, extends(dt) :: et(l)      ! Error 1
    integer, kind :: l
  end type

  type, extends(dt) :: et2
    integer :: k                   ! Error 2
  end type

  type, extends(dt) :: et3
  contains
    procedure, nopass :: k => foo  ! Error 3
  end type

  type, extends(dt) :: et4
  contains
    procedure, nopass :: foo
    generic :: k => foo            ! Error 4
  end type

  type :: dt2(k)
    integer, kind :: k
  contains
    procedure, nopass :: k => foo  ! Error 5
  end type

  type :: dt3(l)
    integer, len :: l
  contains
    procedure, nopass :: foo
    generic :: l => foo            ! Error 6
  end type
contains
  subroutine foo()
  end subroutine
end module
    
