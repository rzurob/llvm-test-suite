! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/06/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               C928: format presence when DECIMAL= specifier in
!                               data transfer statement:
!                               Use of label, * or format-control-specifier for
!                               the IO format; also use deferred character data
!                               as the internal file.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data
    end type

    contains

    character(5) function getPart (s, i)
        integer, intent(in) :: i
        character(5), intent(in) :: s(0:1)

        getPart = s(mod(i,2))
    end function

    subroutine appendVal (c, val)
        character(:), allocatable, intent(inout) :: c
        class(*), intent(in) :: val

        character(:), allocatable, save :: localString

        character(*), parameter :: decimalMode = 'POINTCOMMA'

100     format (a,d15.6)

        if (.not. allocated(c)) allocate(c, source=' ')

        select type (val)
            type is (real(8))
                allocate(character(len(c)+15) :: localString)

                write (localString, fmt=100, &
                        decimal=getPart(decimalMode,1)) c, val

            type is (complex(8))
                allocate(character(len(c)+66) :: localString)

                write (localString, fmt=*, decimal=getPart(decimalMode,191), &
                    sign='PLUS', round='UP') c, val

            class default
                allocate(character(len(c)+13) :: localString)

                write (localString, fmt='(2a)', decimal='COMMA') &
                        c,  ' "other type"'

        end select

        call move_alloc (localString, c)
    end subroutine
end module

program dcmlCharExprRW002
use m
    class(base), allocatable :: b1(:)

    character(:), allocatable :: c1

    double precision d1(100)
    complex(8) cx1(100)
    character(10) restOfString(100)

    logical(4), external :: precision_r8, precision_x6

    allocate (b1(300))

    do i = 1, 100
        allocate(b1(i)%data, source=i*1.0d0)
    end do

    do i = 101, 200
        allocate (b1(i)%data, source=cmplx(i, i+1, 8))
    end do

    do i = 201, 300
        allocate (b1(i)%data, source=mod(i,2)==0)
    end do

    !! write all the data to the string c1
    do i = 1, 300
        call appendVal (c1, b1(i)%data)
    end do

    !! now verify the value of c1
    read (c1, fmt=*, decimal='COMMA') d1, cx1, restOfString

    do i = 1, 100
        if (.not. precision_r8(d1(i), i*1.0d0)) error stop 1_4

        if (.not. precision_x6(cx1(i), cmplx(100+i, 101+i, 8))) &
                error stop 2_4

        if (restOfString(i) /= 'other type') error stop 3_4
    end do
end
