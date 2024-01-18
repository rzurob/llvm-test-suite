! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2005
!*
!*  DESCRIPTION                : allocate (allocation of function return is
!                               illegal)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        procedure :: x => producePtr
    end type

    contains

    integer function producePtr (b)
        class (base) :: b

        pointer producePtr

        nullify (producePtr)
    end function
end module

program falloc510d
use m
    type (base) b1

    allocate (b1%x())           !<-- illegal
    allocate (producePtr (b1))  !<-- illegal
end
