! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/polyFuncRet001.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/25/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               A polymorphic function results that is of
!                               pointer containing allocatable components;
!                               result is a scalar.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), pointer :: ids(:)
    end type

    type, extends(base) :: child(n2,k2)    ! (20,4,20,8)
        integer, kind     :: k2
        integer, len      :: n2
        real(k2), pointer :: data(:,:)
    end type

    contains

    class(base(:,4)) function genBasePtr (ids, data)
        pointer genBasePtr

        integer, intent(in) :: ids(:)
        real(8), intent(in), optional :: data(:,:)


        if (present(data)) then
            allocate (child(20,4,20,8) :: genBasePtr)
        else
            allocate (base(20,4) :: genBasePtr)
        end if

        allocate (genBasePtr%ids(size(ids)))

        genBasePtr%ids = ids

        select type (genBasePtr)
            type is (base(*,4))
            class is (child(*,4,*,8))
                allocate (genBasePtr%data(size(data,1), size(data,2)))
                genBasePtr%data = data

            class default
        end select
    end function
end module

program polyFuncRet001
use m
    type(child(:,4,:,8)), allocatable :: c1
    type(base(:,4)), allocatable :: b2

    b2 = genBasePtr ([(i, i=0, 999)], reshape([real(8)::], [10, 0]))

    select type (x => genBasePtr([(i, i=-1, 998)], reshape([real(8)::], [1000, 0])))
        type is (child(*,4,*,8))
            c1 = x

        class default
            stop 10
    end select

    !! verify b2 and c1
    if ((.not. allocated(b2)) .or. (.not. allocated(c1))) error stop 1_4

    if ((.not. associated(b2%ids)) .or. (lbound(b2%ids, 1) /= 1) .or. &
        (ubound(b2%ids, 1) /= 1000)) error stop 2_4


    if ((.not. associated(c1%ids)) .or. (lbound(c1%ids,1) /= 1) .or. &
        (ubound(c1%ids,1) /= 1000)) error stop 3_4

    do i = 1, 1000
        if (b2%ids(i) /= i - 1) error stop 4_4
        if (c1%ids(i) /= i - 2) error stop 5_4
    end do

    if ((.not. associated(c1%data)) .or. any(lbound(c1%data) /= 1) .or. &
        any(ubound(c1%data) /= [1000, 0])) error stop 6_4

    deallocate(b2%ids, c1%ids, c1%data)
    deallocate(b2, c1)
end
