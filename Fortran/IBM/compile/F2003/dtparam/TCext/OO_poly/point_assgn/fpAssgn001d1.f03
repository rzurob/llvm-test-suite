! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn001d1.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2005
!*
!*  DESCRIPTION                : data pointer assignment (C716 type
!                               compatibility for assignment to happen)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn001d1
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name
    end type

    type (base(:,4)), pointer :: b1
    class (child(:,4,4,:)), pointer :: c1 => null()

    b1 => c1    !<-- illegal
end
