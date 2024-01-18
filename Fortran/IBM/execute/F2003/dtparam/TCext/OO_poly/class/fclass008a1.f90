! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/class/fclass008a1.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2005
!*
!*  DESCRIPTION                : CLASS keyword (test the transfer() intrinsic
!                               that returns poly-data)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name
    end type
end module

program fclass008a1
use m
    class (base(:,4)), pointer :: b1(:)

    type (child(20,4,1)), target :: c1(10)

    c1%id = (/(j, j=1,10)/)
    c1%name = 'test'

    b1 => c1

    associate (x => transfer(b1(::2), c1, 2))
        if (size (x) /= 2) error stop 1_4

        if (any (x%id /= (/1, 3/))) error stop 2_4

        if (any (x%name /= 'test')) error stop 3_4
    end associate


    !! test transfer for poly data
    associate (x => transfer(b1(::2), b1, 2))  !<-- transfer() returns poly data

        if (size(x) /= 2) error stop 4_4

        select type (x)
            type is (child(*,4,1))
                if (any (x%id /= (/1,3/))) error stop 5_4

                if (any (x%name /= 'test')) error stop 6_4
            class default
                error stop 10_4
        end select
    end associate
end
