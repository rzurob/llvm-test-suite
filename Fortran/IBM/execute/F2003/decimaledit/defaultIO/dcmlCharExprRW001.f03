! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/05/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Write and read data from a string (internal
!                               file) in decimal edit mode of COMMA.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharExprRW001
    interface
        subroutine setString (s, val)
            character(*), intent(out) :: s
            class(*), intent(in) :: val(:)
        end subroutine
    end interface

    character(20000) c1
    character(:), allocatable:: c2

    class(*), allocatable :: x1(:)

    logical(4), external :: precision_r4, precision_x8

    real r1(1000)
    complex cx(600)

    allocate(x1(1000), source=(/(1.2e0*i, i=1,1000)/))

    c1(1:5) = 'COMMA'

    allocate(character(6) :: c2)

    call setString (c1(6:), x1)

    read (c1, *, decimal='COMMA') c2

    read (c1(6:), *, decimal=c2) r1

    do i = 1, 1000
        if (.not. precision_r4 (r1(i), 1.2e0*i)) error stop 1_4
    end do

    !! 2nd test
    deallocate (x1)

    allocate (x1(600), source=cmplx((/((i*1.0)**.5, i=1,600)/), &
        sqrt((/(i*1.0, i=0,599)/))))

    call setString (c1(6:), x1)

    read (c1, '(a)') c2

    read (c1(6:), *, decimal=c2) cx

    do i = 1, 600
        if (.not. precision_x8(cx(i), cmplx(sqrt(i*1.0), sqrt(1.0*i-1.0)))) &
                error stop 2_4
    end do
end

subroutine setString (s, val)
    character(*), intent(out) :: s
    class(*), intent(in) :: val(:)

    character(*) commaMode
    parameter (commaMode  =  'COMMA    ')
    integer ipos

    ipos = 1

    do i = 1, size(val)
        select type (x => val(i))
            type is (real)
                write(s(ipos:), '(g12.5)', decimal=commaMode) x

                ipos = ipos + 12

            type is (complex)
                write(s(ipos:ipos+30), *, decimal=''//'CO'//''//''//'MMA') x

                ipos = ipos + 31

            class default
                write (s(ipos:), 20, decimal='comma')

                ipos = ipos + 12

        end select

20  format ('other type')
    end do
end subroutine

