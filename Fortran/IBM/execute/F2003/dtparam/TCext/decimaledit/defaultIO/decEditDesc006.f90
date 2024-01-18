! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/decimaledit/defaultIO/decEditDesc006.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test synchronous IO on a file opened for asynchr
!                               IO; use DP edit descriptor in
!                               WRITE for a file connected under COMMA mode;
!                               read in the data in POINT mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    complex(4), pointer :: cx1(:)
    double precision, pointer :: d8(:)
    integer, pointer :: i1(:)

    type dataType(n1,k1,k2,k3)    ! (20,4,4,8)
        integer, kind :: k1,k2,k3
        integer, len  :: n1
        complex(k1)   :: cx
        integer(k2)   :: id
        real(k3)      :: val
    end type

    type base(k4,n2,k5)    ! (4,20,8)
        integer, kind               :: k4,k5
        integer, len                :: n2
        type(dataType(n2,k4,k4,k5))    data(500)
    end type

    type(base(4,:,8)), allocatable :: b1(:)

    class(*), allocatable :: x(:)
end module

program decEditDesc006
use m
    logical(4), external :: precision_x8, precision_r8

    open (1, file='aWrite.out', asynchronous='yes', decimal='Comma', &
            status='replace')

    allocate (b1(100), source=(/(base(4,20,8)((/(dataType(20,4,4,8)(cmplx(i,-j), i*1000+j, i*j), &
                j=1,500)/)), i=1,100)/))

    call writeAIO (1)

    !! assume the previous write takes awhile; let's set up the pointers for
    !read
    allocate (cx1(100*500), d8(100*500), i1(100*500))

    allocate (x(10000), source=base(4,20,8)(dataType(20,4,4,8)(1,1,1)))

    rewind 1

    open (1, decimal='POINT', sign='plus')

    !! now read in the data
    call readAIO (1)

    !! while it's busy reading, let's clean up
    deallocate (b1)

    deallocate (x)


    !! let's verify the data read in
    k = 1

    do i = 1, 100
        do j = 1, 500
            if (.not. precision_x8(cx1(k), cmplx(i,-j,4))) error stop 1_4

            if (i1(k) /= i*1000+j) error stop 2_4

            if (.not. precision_r8(d8(k), i*j*1.0d0)) error stop 3_4

            k = k + 1
        end do
    end do
end


subroutine writeAIO (unit)
use m
    integer, intent(in) :: unit

    if (.not. allocated(b1)) stop 10

    do i = lbound(b1,1), ubound(b1,1)
        write (unit, fmt='(sp, e15.7, dp, ss, e15.7, dc, sp, i10, d30.15)') b1(i)
    end do
end subroutine


subroutine readAIO (unit)
use m
    integer, intent(in) :: unit


    if ((size(cx1) /= 100*500) .or. (size(d8) /= 100*500) .or. &
        (size(i1) /= 100*500)) &
        stop 20


    do i = 1, 100*500
        read (unit, fmt= &
            '(dc, e15.7, dp, ss, e15.7, sp, i10, dc, d30.15)') cx1(i), i1(i), d8(i)
    end do
end subroutine
