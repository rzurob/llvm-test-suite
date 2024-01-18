include "ftpbnd509a1.hf"

subroutine printBase (b)
use m
    class (base(*,4)), intent(in) :: b

    print *, b%id
end subroutine
