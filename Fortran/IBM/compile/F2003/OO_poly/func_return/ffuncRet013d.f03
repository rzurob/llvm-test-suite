! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2005
!*
!*  DESCRIPTION                : poly function return (recursive keyword and
!                               poly function results; diagnostic case: default
!                               IO on poly-function results)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) :: data = -1
    end type

    contains

    recursive class(base) function test1 ()
        pointer test1

        allocate (test1)
    end function

    recursive class(base) function test2 ()
        allocatable test2

        allocate (test2)
    end function
end module

program ffuncRet013d
use m
    print *, test1()    !<-- illegal
    print *, test2()    !<-- illegal
end