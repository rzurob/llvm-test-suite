! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr041d1.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 0

    end type
end module

program fconstr041d1
use m
    implicit type (base(4)) (b), class (base(4)) (x), class (*) (z)

    print *, z1, x1  !<-- x1 and z1 illegal without POINTER or ALLOCATABLE attributes

    print *, b1

    x1 = base(4) (100)
    z1 = base(4) (10)
end

