include "ftpbnd509a1.hf"

subroutine printBase (b)
use m
    class (base), intent(in) :: b

    print *, b%id
end subroutine
