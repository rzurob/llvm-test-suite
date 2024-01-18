! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/OO_poly/allocate/falloc018a1_1.f90
! opt variations: -qnock -qnol -qnodeferredlp

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n2)    ! (20,4,1,15)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name

        contains

        procedure :: print => printChild
    end type

    class (base(:,4)), allocatable :: b1_m, b2_m(:)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module
