! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn009a5.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2005
!*
!*  DESCRIPTION                : data pointer assignment (sequence type pointer
!                               assigned to unlimted poly targets)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface makeData
        module procedure makeSeqData1
        module procedure makeSeqData2
    end interface

    contains

    class(*) function makeSeqData1 (i1)
        pointer makeSeqData1
        integer(8), intent(in) :: i1(:)

        type seq1(n1,k1)    ! (20,8)
            integer, kind :: k1
            integer, len  :: n1
            sequence
            integer(k1)      i, j
        end type

        nullify (makeSeqData1)

        if (size(i1) == 2) then
            allocate (makeSeqData1, source=seq1(20,8)(i1(1), i1(2)))
        end if
    end function

    class(*) function makeSeqData2 (i1, isize)
        pointer makeSeqData2 (:)
        integer(8), intent(in) :: i1(:)
        integer, intent(in) :: isize

        type seq1(n2,k2)    ! (20,8)
            integer, kind :: k2
            integer, len  :: n2
            sequence
            integer(k2)      i, j
        end type

        nullify (makeSeqData2)

        if (size(i1) == 2) then
            allocate (makeSeqData2(isize), source=seq1(20,8)(i1(1), i1(2)))
        end if
    end function
end module

program fpAssgn009a5
use m
    type seq1(n3,k3)    ! (20,8)
        integer, kind :: k3
        integer, len  :: n3
        sequence
        integer(k3)      i, j
    end type

    type(seq1(:,8)), pointer :: s1, s2(:)

    !! first test
    s1 => makeData((/1_8, 10_8/))

    s2 => makeData((/-1_8, -10_8/), 2)

    if ((.not. associated(s1)) .or. (.not. associated(s2))) error stop 1_4

    if ((s1%i /= 1) .or. (s1%j /= 10)) error stop 2_4

    if (size(s2) /= 2) error stop 3_4

    if (any(s2%i /= -1) .or. any (s2%j /= -10)) error stop 4_4

    deallocate (s1, s2)

    !! 2nd test
    s1 => makeData ((/1_8/))
    s2 => makeData ((/-1_8/), 10)

    if (associated (s1) .or. associated(s2)) error stop 5_4
end
