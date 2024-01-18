! specific procedure for an operator must not have an optional arg.

module mod
  type dt
    integer i
  contains
    procedure :: addthem
    generic :: operator(//) => addthem
  end type
contains
  pure integer function addthem(a, b)
    class(dt), intent(in) :: a
    integer, optional,  intent(in) :: b
    addthem = a%i + b + 1
  end function

  subroutine sub
    type(dt) :: dta
    dta%i = 3
    i = dta // 4
  end subroutine
end module

use mod
call sub
end
