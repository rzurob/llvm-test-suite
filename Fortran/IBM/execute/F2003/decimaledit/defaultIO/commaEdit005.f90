!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of direct access mode and data edit
!                               descriptor of ES and EN
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program commaEdit005
    real(4) r1(10)
    real(4), allocatable :: r2(:)

    complex(16) :: cx1(10), cx2(:)

    pointer cx2

    character(30) fmt

    logical(4) precision_r4, precision_x6

    interface
        subroutine readDataDirect (unit, recNum, fmt, x)
            integer, intent(in) :: unit, recNum
            character(*), intent(in) :: fmt
            class(*), intent(out) :: x(:)
        end subroutine

        subroutine writeDataDirect (unit, recNum, fmt, x)
            integer, intent(in) :: unit, recNum
            character(*), intent(in) :: fmt
            class(*), intent(out) :: x(:)
        end subroutine
    end interface


    r1 = sqrt((/(i*1.0, i=1,10)/))

    cx1 = cmplx(qsqrt((/(i*1.0q0, i=1,10)/)), 1.0q0, 16)

    ! open a file and write the F formatted data in it
    open (1, form='formatted', access='direct', file='test1', recl=1000, &
            decimal='COMMA')


    write (fmt, *) '(5F15.5)'

    write (1, rec=4, fmt=fmt) r1    !<-- this come across 2 records

    write (1, rec=1, fmt=100) cx1   !<-- this occupies 3 records


    !! now allocate r2 and cx2 for the data to be read in
    allocate (r2(10), cx2(10))

    fmt = '(-2P, 5ES15.5)'

    call readDataDirect (1, 4, fmt, r2)

    call readDataDirect (1, 1, fmt, cx2)

    !! now verify the results
    do i = 1, 10
        if (.not. precision_r4 (r2(i), sqrt(i*1.0)*1.0e2)) error stop 1_4

        if (.not. precision_x6 (cmplx(cx2(i), kind=8), &
            cmplx(dsqrt(i*1.0d0)*1.0d-4, 1.0d-4, 8))) error stop 2_4
    end do

    !! now write out the results in EN format

    open (1, blank='ZERO')

    call writeDataDirect (1, 1, fmt='(5EN15.2)', x=r2)
    call writeDataDirect (1, recNum=3, fmt='no effects', x=cx2)

100 format (4(F30.16, ";", F30.16))
end


subroutine readDataDirect (unit, recNum, fmt, x)
    integer, intent(in) :: unit, recNum
    character(*), intent(in) :: fmt
    class(*), intent(out) :: x(:)

    select type (x)
        type is (real(4))
            read (unit, rec=recNum, fmt=fmt) x

        type is (complex(16))
            read (unit, rec=recNum, fmt='(4P, 4(ES30.16,";",ES30.16))') x

        class default
            error stop 10_4
    end select
end subroutine


subroutine writeDataDirect (unit, recNum, fmt, x)
    integer, intent(in) :: unit, recNum
    character(*), intent(in) :: fmt
    class(*), intent(out) :: x(:)

    select type (x)
        type is (real(4))
            write (unit, rec=recNum, fmt=fmt) x

        type is (complex(16))
            write (unit, rec=recNum, fmt='(4(EN30.16,";",EN30.16))') x

        class default
            error stop 20_4
    end select
end subroutine
