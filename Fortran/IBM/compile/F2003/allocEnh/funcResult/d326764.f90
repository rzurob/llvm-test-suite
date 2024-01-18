!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/17/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 326764)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    abstract interface
        function absInterf (p)
            real p(:)

            real, allocatable :: absInterf(:)
        end function
    end interface

    contains

    function genPtr (p)
        real p(:)

        real, allocatable :: genPtr(:)

        genPtr = p
    end function

    function genProcPtr (p)
        procedure(absInterf), pointer:: p
        procedure(absInterf), pointer :: genProcPtr

        if (associated(p)) then
            genProcPtr => p
        else
            genProcPtr => genPtr
        end if
    end function
end module

use m
    procedure(absInterf), pointer :: p
    procedure(absInterf) :: reverseOrder

    real, allocatable :: rp(:)

    rp = [(i, i=1,10)]

    p => genProcPtr (reverseOrder)  !<-- illegal

end


function reverseOrder (p)
    real p(:)

    real, allocatable :: reverseOrder(:)

    reverseOrder = p(size(p):1:-1)
end function
