! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd504.f
! opt variations: -qck -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2005
!*
!*  DESCRIPTION                : specific type bound (private binding name
!                               overridden to be public)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure, private :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

end module

module m1
use m
    type, extends (base) :: child    ! (20,4)
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child(*,4)), intent(in) :: b

        print *, 'Child'
        print *, b%id, b%name
    end subroutine
end module


program ftpbnd504
use m1
    class (child(:,4)), allocatable, target :: c1(:)
    class (base(:,4)), pointer :: b(:)

    allocate (child(20,4) :: c1(10))

    c1%id = (/(i, i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i), i=1,10)/)

    b => c1(::2)

    select type (b)
        type is (child(*,4))
            if ((lbound(b,1) /= 1) .or. (ubound(b,1) /= 5)) error stop 1_4

            do i = 1, 5
                call b(i)%print
            end do
        class default
            error stop 2_4
    end select
end
