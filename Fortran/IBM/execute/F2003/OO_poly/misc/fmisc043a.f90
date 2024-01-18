!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/25/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 311929)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    real function f1 (r1)
        pointer :: f1

        allocate (f1, source=r1)
    end function

    function genF1 (i)
        procedure (f1), pointer :: genF1

        if (i < 1) then
            nullify (genF1)
        else
            genF1 => f1
        end if
    end function
end module

program fmisc043a
use m
    call test1(genF1(1))
    call test1(genF1(-1))

    contains

    subroutine test1 (p)
        procedure(f1), pointer :: p

        print *, associated(p), associated(p, f1)

        if (associated (p)) then
            write (*, '(f10.1)') p(10.1)
        end if
    end subroutine
end
