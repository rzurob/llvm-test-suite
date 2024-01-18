! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc509a.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2005
!*
!*  DESCRIPTION                : allocate (data allocation in select type
!                               construct)
!*
!*  KEYWORD(S)                 :
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
        integer(k1)      id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,16)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    subroutine copy (x1, x2)
        class (*), allocatable, intent(out) :: x1(:)
        class (*), intent(in) :: x2(:)

        select type (x2)
            type is (real)
                allocate (x1(size(x2)), source=int(x2))
            type is (complex)
                allocate (x1(size(x2)), source=int(x2))
            class default
                allocate (x1(size(x2)), source=x2)
        end select
    end subroutine

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (allocated (x)) then
            print *, 'bounds: ', lbound(x), ubound(x)
            select type (x)
                type is (base(4))
                    print *, x
                type is (child(4,1,*))
                    print *, x
                type is (integer)
                    print *, x
                class default
                    print *, 'other data type'
            end select
        end if
    end subroutine
end module

program falloc509a
use m
    class (*), allocatable :: x1(:)

    integer i1(10)
    complex, pointer :: cx (:)

    class (base(4)), allocatable :: b1(:)

    !! test real data type
    call copy (x1, (/1.0, 3.0/))

    call printX (x1)

    !! test integer type
    i1 = (/(i, i=1,10)/)

    call copy (x1, i1(8::2))

    call printX (x1)

    !! test the complex type
    allocate (cx(0:1), source=(/(-1.0, 2.0), (3.1, -1.5)/))

    call  copy (x1, cx)

    call printX (x1)

    !! test derived types
    allocate (b1(3), source=(/child(4,1,16)(1, 'xlftest1'), child(4,1,16)(2, 'xlftest2'), &
                    child(4,1,16)(3, 'xlftest3')/))

    call copy (x1, b1)

    call printX (x1)
end
