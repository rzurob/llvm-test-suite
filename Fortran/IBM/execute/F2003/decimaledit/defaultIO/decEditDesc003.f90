! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/18/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test DC/DP in write on the internal file; also test
!                               the use of deferred character in a loop that
!                               changes the length of the string during the
!                               execution.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        real(4) data(2)
        complex :: cx
        logical :: flag(2)
    end type

    type base
        type(dataType) :: data(2)
    end type

    contains

    character(:) function genString (b)
        type(base), intent(in) :: b(:)

        allocatable :: genString, localAlloc

        character(:) localAlloc

        character(2000) localBuffer

        do i = 1, size(b)
            localBuffer=''
            write (localBuffer, 100) b(i)

            if (i == 1) then
                allocate (localAlloc, source=trim(localBuffer))
            else
                allocate (localAlloc, source=genString//';'//trim(localBuffer))
            end if

            call move_alloc(localAlloc, genString)
        end do

100 format (Dc, 2(g10.3, ';', g10.3, '; (', g10.3, ';', g10.3, '); ', &
        2(L10, 1x)))
    end function
end module

program decEditDesc003
use m
    type(base), allocatable :: b1(:), b2(:,:)

    logical(4), external :: precision_r4, precision_x8

    allocate (b1(10), b2(2,5))

    b1 = (/(base((/(dataType((/i*10+j, i*10+j+1/), cmplx(i*10+j, i*10+j+1), &
            mod(i,j) == 0), j=1,2)/)), i=1, 10)/)

    associate (str => genString (b1))
        read (str, *, decimal='coMMa', iostat=istat) b2

        if (istat /= 0) stop  10
    end associate

    k = 1

    do j = 1, 5
        do i = 1, 2
            do n1 = 1, 2
                if (.not. precision_r4(b2(i,j)%data(n1)%data(1), k*1.0e1+n1)) &
                    error stop 1_4

                if (.not. precision_r4(b2(i,j)%data(n1)%data(2), k*1.0e1+n1+1)) &
                    error stop 2_4

                if (.not. precision_x8(b2(i,j)%data(n1)%cx, &
                        cmplx(k*10+n1,k*10+n1+1,4))) error stop 3_4

                if (.not. all (b2(i,j)%data(n1)%flag .eqv. mod(k,n1) == 0)) &
                    error stop 4_4
            end do

            k = k + 1
        end do
    end do
end
