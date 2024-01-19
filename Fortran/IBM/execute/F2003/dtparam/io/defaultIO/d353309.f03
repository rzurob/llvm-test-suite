! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2009
!*
!*  DESCRIPTION                : miscellaneous (IO: defect 353309)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type X (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    contains

    subroutine t (x1)
        type(X(4, *)) :: x1(:)

        namelist /nml/ x1

        write (*, nml)
    end subroutine
end module

use m
    type(X(4, 2)) :: x1(2)

    x1(1)%data = [1,2]
    x1(2)%data = [10,20]

    call t (x1)
end

