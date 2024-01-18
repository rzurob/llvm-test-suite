! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet013a1.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2005
!*
!*  DESCRIPTION                : poly-function results (recursive function
!                               return)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind     :: k1
        integer, len      :: n1
        real(k1), private :: data

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base(*,8)), intent(in) :: b

        write (*, '(e15.5)') b%data
    end subroutine

    recursive class(base(:,8)) function sum1 (n)
        allocatable sum1
        integer, intent(in) :: n

        real(8) :: seed = 1.0_8

        if (n <= 1) then
            seed = 1.0_8
            allocate (sum1, source=base(20,8)(1.0_8))
        else
            associate (x => sum1 (n-1))
                seed = seed + 1.0_8

                allocate (sum1, source=base(20,8)(seed+x%data))
            end associate
        end if
    end function
end module

program ffuncRet013a1
use m
    class(base(:,8)), allocatable :: b1

    associate (x => sum1(1000))
        call x%print
    end associate

    allocate (b1, source=sum1(20000))

    call b1%print
end
