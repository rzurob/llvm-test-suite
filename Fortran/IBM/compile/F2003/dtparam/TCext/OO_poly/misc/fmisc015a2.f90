! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/misc/fmisc015a2.f
! opt variations: -qnol -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2005
!*
!*  DESCRIPTION                : miscellaeous items
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc015a2
    type base(n1,k1,k2,k3)    ! (20,8,4,4)
        integer, kind         :: k1,k2,k3
        integer, len          :: n1
        integer(k1)              i1
        real(k2), allocatable :: r1
        integer(k3), pointer  :: i2
    end type

    type (base(20,8,4,4)) b1

    b1=base(20,8,4,4) (100, 1.3, null())

    associate (x => transfer(b1, b1, 1))
        x%i1 = 10                       !<-- illegal

        deallocate (x(1)%r1)            !<-- illegal
        allocate (x(1)%i2, source=100)  !<-- illegal

    end associate
end
