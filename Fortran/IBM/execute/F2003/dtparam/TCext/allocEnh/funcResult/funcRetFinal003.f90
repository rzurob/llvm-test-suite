! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/funcResult/funcRetFinal003.f
! opt variations: -ql

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 11/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that no finalization for function result if
!                               function return is a pointer; test for rank 3
!                               arrays.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: data(:)

        contains

        procedure, nopass :: makeArrayRank3
        final :: finalizeBaseRank3
    end type

    contains

    subroutine finalizeBaseRank3 (b1)
        type(base(4)), intent(inout) :: b1(:,:,:)

        print *, 'finalizeBaseRank3:', shape(b1)

        do i = 1, size(b1,1)
            do j = 1, size(b1, 2)
                do k = 1, size(b1, 3)
                    if (associated(b1(i,j,k)%data)) deallocate (b1(i,j,k)%data)
                end do
            end do
        end do
    end subroutine

    function makeArrayRank3 (source, l, m, n, sizes)
        integer, intent(in) :: l,m,n, source(*), sizes(l,m,n)

        type(base(4)), pointer :: makeArrayRank3(:,:,:)
        integer :: currentPos

        currentPos = 1

        allocate(makeArrayRank3(l,m,n))

        do k = 1, n
            do j = 1, m
                do i = 1, l
                    allocate (makeArrayRank3(i,j,k)%data(sizes(i,j,k)))

                    makeArrayRank3(i,j,k)%data = &
                        source(currentPos:currentPos+sizes(i,j,k)-1)

                    currentPos = currentPos+sizes(i,j,k)
                end do
            end do
        end do
    end function
end module


program funcRetFinal003
use m
    type (base(4)), allocatable :: b1(:,:,:)

    integer, allocatable :: i1(:), sizes(:,:,:)

    allocate (b1(0:2, 0:3, 0:4))

    i1 = [(i, i=1, 10000)]

    sizes = reshape([(i, i=1, 8000)], [4,5,6])

    do i = 0, 2
        do j = 0, 3
            do k = 0, 4
                allocate (b1(i,j,k)%data(i+j+k))
            end do
        end do
    end do

    !! finalization happens only for b1 during its deallocation
    b1 = b1(0,0,0)%makeArrayRank3 (i1, 4,5,6, sizes)

    !! veridy b1
    if (any(lbound(b1) /= 1) .or. any(ubound(b1) /= [4,5,6])) error stop 1_4

    iPos = 1
    isize = 1

    do k = 1, 6
        do j = 1, 5
            do i = 1, 4
                if (.not. associated(b1(i,j,k)%data)) error stop 2_4

                if (size(b1(i,j,k)%data) /= isize) error stop 3_4

                if (any(b1(i,j,k)%data /= [(l, l=iPos, iPos+isize-1)])) &
                    error stop 4_4


                iPos = iPos + isize

                isize = isize + 1
            end do
        end do
    end do


    !! 2nd assignment with no reallocation to happen; only one finalization for
    !b1; the function result is not finalized
    b1 = b1(3, 2, 3)%makeArrayRank3 (i1(10000:1:-1), 4,5,6, [(j, j=120,1,-1)])

    !! veridy b1
    if (any(lbound(b1) /= 1) .or. any(ubound(b1) /= [4,5,6])) error stop 5_4

    iPos = 10000
    isize = 120

    do k = 1, 6
        do j = 1, 5
            do i = 1, 4
                if (.not. associated(b1(i,j,k)%data)) error stop 6_4

                if (size(b1(i,j,k)%data) /= isize) error stop 7_4

                do l = 1, isize
                    if (b1(i,j,k)%data(l) /= iPos-l+1) error stop 8_4
                end do


                iPos = iPos - isize

                isize = isize - 1
            end do
        end do
    end do

end
