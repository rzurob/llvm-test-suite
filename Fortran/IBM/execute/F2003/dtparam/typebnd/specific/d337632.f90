!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/06/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 337632)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k
        integer, len :: n

        complex(k) data(n)

        character(:), allocatable :: str
    end type
end module


use m
    type(base(8,:)), allocatable :: b1

    complex(8), allocatable :: cx(:)

    integer isize
    logical, external :: precision_x6

    isize = 10

    cx = cmplx([(sin(i*1.2d-2), i=1,isize)],  &
        [(cos(i*1.2d-2), i=1,isize)], kind = 8)

    allocate (base(8,isize) :: b1)

    b1%data = cx

    allocate (character(10*30)::b1%str)

    write (b1%str, '(20f15.10)') b1%data

    print *, b1%str
end
