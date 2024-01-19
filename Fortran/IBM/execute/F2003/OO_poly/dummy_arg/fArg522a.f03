! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2005
!*
!*  DESCRIPTION                : argument association (test of reasonably
!                               sized-arrays)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    !! do sum for integer, real, complex and character types
    subroutine sum1 (x1, x2)
        class (*), intent(out), allocatable :: x1
        class (*), intent(in) :: x2(:)

        character(200) err
        integer charLen

        select type (x2)
            type is (integer)
                !! get a 8-byte integer sum
                allocate (x1, source=sum(int(x2, 8)))
            type is (real)
                !! get a 8-byte real sum
                allocate (x1, source=sum(real(x2, 8)))
            type is (complex)
                !! get a 16-byte complex sum
                allocate (x1, source=sum(cmplx(x2, kind=8)))
            type is (character(*))
                !! concatenate all the elements which are trimmed off
                charLen = size(x2)*(len(x2) + 1)

                allocate (character(charLen) :: x1, stat=i3, errmsg=err)

                if (i3 /= 0) then
                    print *, i3, err
                    error stop 15_4
                end if

                select type (x1)
                    type is (character(*))
                        x1 = ''

                        do i = 1, size (x2)
                            x1 = trim(x1) // ' ' // x2(i)
                        end do
                    class default
                        error stop 11_4
                end select
            class default
                error stop 12_4
        end select
    end subroutine

    subroutine printX (x)
        class (*), intent(in) :: x

        select type (x)
            type is (integer(8))
                print *, x
            type is (real(8))
                write (*, '(g12.4)') x
            type is (complex(8))
                write (*, '(2g12.4)') x
            type is (character(*))
                print *, x
            class default
                error stop 10_4
        end select
    end subroutine
end module

program fArg522a
use m
    class(*), allocatable :: x
    integer, parameter :: LIMIT = 5000000

    allocate (x, source= 100)

    call sum1 (x, (/(i, i=1, LIMIT)/))

    call printX (x)

    call sum1 (x, (/(i*1.0, i=1,LIMIT)/))

    call printX (x)

    call sum1 (x, (/((i*1.0, i*1.0), i=1, LIMIT)/))

    call printX (x)

    call sum1 (x, (/(i2a(i), i = 1, 10)/))

    call printX (x)

    call sum1 (x, (/(i2a(i), i=1, LIMIT)/))

    select type (x)
        type is (character(*))
            call printX (x(100:200))
        class default
            error stop 1_4
    end select

    contains

    character(7) function i2a (i)
        integer,intent(in) :: i

        write (i2a, '(i7)') i
    end function
end
