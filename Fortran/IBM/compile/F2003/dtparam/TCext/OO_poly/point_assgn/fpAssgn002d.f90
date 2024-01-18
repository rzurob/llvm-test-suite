! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn002d.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2005
!*
!*  DESCRIPTION                : data pointer assignment (C717)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn002d
    type x(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      x1
    end type

    class (*), pointer :: x1

    class (x(:,4)), pointer :: x2

    x1 => null()

    x2 => x1        ! this is not allowed
end
