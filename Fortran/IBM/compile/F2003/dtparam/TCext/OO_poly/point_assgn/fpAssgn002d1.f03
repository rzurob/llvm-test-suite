! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn002d1.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : data pointer assignment (C610: component name
!                               is based on the declared type of the data
!                               object)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn002d1

    type p(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      abc
    end type

    class(*), pointer :: x
    type (p(4)), target :: p1

    x => p1

    print *, x%abc      !! illegal data-ref
end
