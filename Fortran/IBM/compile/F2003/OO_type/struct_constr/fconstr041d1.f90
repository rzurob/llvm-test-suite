!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2005
!*
!*  DESCRIPTION                : structure constructor (implicit statment using
!                               CLASS keyword)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) :: id = 0

    end type
end module

program fconstr041d1
use m
    implicit type (base) (b), class (base) (x), class (*) (z)

    print *, z1, x1  !<-- x1 and z1 illegal without POINTER or ALLOCATABLE attributes

    print *, b1

    x1 = base (100)
    z1 = base (10)
end

