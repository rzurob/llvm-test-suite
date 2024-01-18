!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/19/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test DC/DP in READ statement from a scratch
!                               file; also test the value separator (; in comma
!                               mode) in various data types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer, allocatable :: i1(:)

    character(:), allocatable :: c(:)

    real(8), allocatable :: r1(:,:)

    complex(4), allocatable :: cx(:)

    logical(4), allocatable :: l1(:)

    contains

    function genStr (i)
        character(35) genStr

        write (genStr, '(DC,a,";",e10.2,";",e10.2)') 'xlf', cmplx(i, i)
    end function
end module

program decEditDesc004
use m
    open (1, form='formatted', status='scratch', access='stream')

    write (1, 100) (j, j = 1, 100)

    write (1, 200) (genStr(j), j = 1, 100)

    write (1, 300) (sin(j*1.0), j=1, 100)

    write (1, 400) (cmplx(j,j), j=1, 100)

    write (1, 500)

    rewind 1

    call readVars (100, 1)

    call verifyVars

    close(1)

100 format (DC, 1x, 100(i3, ";"))
200 format (DC, 1x, 100a)
300 format (DC, 1x, 100(f10.3, ";"))
400 format (DC, 1x, 100(e10.3, ";", e10.3, ";"))
500 format (DC, 1x, 50('.T;F.;'))

end


subroutine readVars (isize, unit)
use m
    integer, intent(in) :: isize, unit

    if (allocated (i1)) deallocate(i1)

    if (allocated (c)) deallocate (c)

    if (allocated (r1)) deallocate (r1)

    if (allocated (cx)) deallocate (cx)

    if (allocated (l1)) deallocate (l1)

    allocate (i1(isize), r1(2,isize/2), cx(isize), l1(isize))

    allocate (character(35) :: c(isize))

    read (unit, '(dc, 100i10)') i1
    read (unit, '(dc, 1x, 100a)') c
    read (unit, '(dc, 100f15.3)') r1
    read (unit, '(dc, 100(e15.3,e15.3))') cx

    read (unit, '(dc, 100l5)') l1

end subroutine


subroutine verifyVars
use m
    logical(4), external :: precision_x8

    character*(50) str1, str2

    do i = 1, 100
        if (i1(i) /= i) error stop 1_4

        if (c(i) /= genStr(i)) error stop 2_4

        write (str1, '(e10.3, e10.3)', decimal='Comma') cx(i)
        write (str2, '(dc, e10.3, e10.3)') cmplx(i,i)

        if (str1 /= str2) error stop 3_4

        if (l1(i) .neqv. mod(i,2) == 1) error stop 4_4
    end do

    k = 1

    do j = 1, 50
        do i = 1, 2
            write (str1, '(f12.3)', decimal='commA') r1(i,j)
            write (str2, '(dc, f12.3)') sin(k*1.0)

            if (str1 /= str2) error stop 5_4

            k = k + 1
        end do
    end do
end subroutine
