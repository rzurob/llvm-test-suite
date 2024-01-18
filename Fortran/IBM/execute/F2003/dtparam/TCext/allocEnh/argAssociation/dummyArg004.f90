! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/F2003/allocEnh/argAssociation/dummyArg004.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/14/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the dummy-arg with VALUE attribute used as
!                               the expr in the intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable :: name
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1), allocatable :: id
    end type

    contains

    subroutine test1 (b1, b2)
        type(child(4,*)), allocatable :: b1
        type(child(4,20)), value :: b2

        b1 = b2
    end subroutine

    subroutine test2 (b1, b2)
        type(child(4,:)), allocatable, intent(out) :: b1(:)
        class(base(4,*)), intent(in) :: b2(:)

        select type (b2)
            class is (child(4,*))
                b1 = b2

        end select
    end subroutine
end module

program dummyArg004
use m
    type(child(4,20)), allocatable :: c1
    type(child(4,:)), allocatable :: c2(:)

    class(base(4,:)), allocatable :: b3(:)

    allocate (b3(0:99), source=(/(child(4,20)(name=repeat(achar(65+i/2), i), &
        id=i), i=1,100)/))

    call test1 (c1, child(4,20)('xlftest', -100))

    call test2 (c2, b3(::2))

    !! verify c1, c2
    if ((.not. allocated(c1)) .or. (.not. allocated(c2))) error stop 1_4

    if (size(c2) /= 50) error stop 2_4

    if ((.not. allocated(c1%name)) .or. (.not. allocated(c1%id))) error stop 3_4

    if ((c1%name /= 'xlftest') .or. (c1%id /= -100)) error stop 4_4

    do i = 1, 50
        if ((.not. allocated(c2(i)%name)) .or. (.not. allocated(c2(i)%id))) &
                error stop 5_4

        if (c2(i)%name /= repeat(achar(65+i-1), 2*i-1)) error stop 6_4

        if (c2(i)%id /= 2*i-1) error stop 7_4
    end do

    !! now deallocate c2
    call test2(c2, (/base(4,20)::/))

    if (allocated(c2)) error stop 8_4
end

