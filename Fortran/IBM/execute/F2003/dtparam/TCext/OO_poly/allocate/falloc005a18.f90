! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a18.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a18.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (defined binary operator (.union.)
!                               used as the source-expr)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: data
    end type

    contains

    !! this function finds out how many elements for b1 and b2 are the same,
    !where equal value of data is considered to be the same
    pure integer(4) function inCommon (b1, b2)
        class (base(4)), intent(in) :: b1(:), b2(:)

        inCommon = 0

        do i = 1, size (b1)
            do j = 1, size (b2)
                if (b1(i)%data == b2(j)%data)  inCommon = inCommon + 1
            end do
        end do
    end function
end module

program falloc005a18
use m
    interface operator (.union.)
        type (base(4)) function unionB1B2 (b1, b2)
        use m
            class (base(4)), intent(in) :: b1(:), b2(:)
            dimension unionB1B2 (size(b1)+size(b2)-inCommon(b1,b2))
        end function
    end interface

    type (base(4)) :: b1(4), b2(2), b11(5), b21(4)

    class (base(4)), allocatable :: b3(:)
    type (base(4)), pointer :: b4(:)

    b1%data = (/1,1,2,3/)
    b2%data = (/1, 2/)

    b11%data = (/4,2,3,1,4/)
    b21%data = (/10,3,2,8/)

    !! get the unique union of b1 and b2
    allocate (b3(size(b1)+size(b2)-inCommon(b1,b2)), source=b1 .union. b2)

    !! get the unique union of b11(2:5) and b21(1:2)
    allocate (b4(size(b11(2:))+size(b21(:2))-inCommon(b11(2:), b21(:2))), &
            source=(b11(2:) .union. b21(:2)))

    if (any (b4%data /= (/2,3,1,4,10/))) error stop 1_4
    if (any (b3%data /= (/1,2,3/))) error stop 2_4

    deallocate (b3, b4)
end


!! find the unique union set of b1 and b2; not sorting is applied
type (base(4)) function unionB1B2 (b1, b2)
use m
    class (base(4)), intent(in) :: b1(:), b2(:)
    dimension unionB1B2 (size(b1)+size(b2)-inCommon(b1,b2))

    integer(4) :: index1, merge1 (size(b1)+size(b2))

    index1 = 1

    !! merge two arrays first
    merge1 = (/b1%data, b2%data/)


    !! then we take only the unique value set
    unionB1B2(1)%data = merge1(1)

    i_do: do i = 2, size(merge1)
        j_do: do j = 1, index1
            if (merge1(i) == unionB1B2(j)%data)     cycle i_do
        end do j_do

        index1 = index1 + 1
        unionB1B2(index1)%data = merge1(i)
    end do i_do
end function
