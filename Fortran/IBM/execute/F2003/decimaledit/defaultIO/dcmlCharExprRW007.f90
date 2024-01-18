! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/27/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the namelist IO in the asynchronous IO
!                               mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex(8) :: cx
        integer :: id
        logical :: flag
    end type

    character(:), allocatable :: names(:)
    real(4) :: r1(200,100)
    type (base), allocatable :: b1(:)
end module

module n
use m, only: base
use m, r2 => r1, Mnames => names, b2 => b1

    character(:), allocatable :: names (:)

    real(4) :: r1(200, 100)
    type (base), allocatable :: b1(:)

    namelist /nml1/ names, b1, r1

    contains

    subroutine readNml (unit)
        integer, intent(in) :: unit

        rewind (unit)

        read (unit, nml=nml1, asynchronous='yes')
    end subroutine

    subroutine cmpVars
        logical(4), external :: precision_r4, precision_x6

        do i = 1, 100000
            if (names(i) /= Mnames(i)) error stop 1_4
        end do

        do i = 1, 200
            do j = 1, 100
                if (.not. precision_r4(r1(i, j), r2(i, j))) error stop 2_4

                if (.not. precision_r4(r1(i, j), real(i*1000 + j, 4))) &
                        error stop 3_4
            end do
        end do

        do i = 1, 10000
            if (b1(i)%id /= i) error stop 4_4

            if (.not. precision_x6(b1(i), cmplx(i, kind=8))) error stop 5_4

            if (b1(i)%flag .neqv. mod(i,2) == 0) error stop 6_4
        end do
    end subroutine
end module

program dcmlCharExprRW007
use m
    namelist /nml1/ names, b1, r1

    call setData


    open (1, file='dcmlCharExprRW007.data', asynchronous='yes', delim='quote',&
            decimal='Comma', sign='plus')

    write (1, nml=nml1, asynchronous='yes')

    !! while it's busy writing data Async, let's allocate memory for module n
    call allocateNData

    wait (1)
    call readNData(1)

    call verifyData
end

subroutine setData
use m

    names = (/(seqNum(i/=i), i=1, 100000)/)

    do i = 1, 200
        do j = 1, 100
            r1(i,j) = i*1000 + j
        end do
    end do

    allocate (b1(10000), source=(/(base(cmplx(i), i, mod(i,2) == 0), i=1, 10000)/))

    contains

    function seqNum (resetFlag)
        logical, intent(in) :: resetFlag
        character(10) seqNum

        integer :: seq = 0

        if (resetFlag) then
            seq = 1
        else
            seq = seq + 1
        end if

        write (seqNum, '(a,i6)') 'seq', seq
    end function
end subroutine


subroutine allocateNData
use n
    if (.not. allocated(names)) allocate (character(10) :: names(size(Mnames)))
    if (.not. allocated(b1)) allocate (b1(size(b2)))

    r1 = -1.0
    names(:) = ''
    b1 = base ((0.0, 0.0), -1, .false.)
end subroutine


subroutine readNData (unit)
use n
    integer, intent(in) :: unit

    call readNml (unit)
end subroutine

subroutine verifyData
use n
    call cmpVars
end subroutine
