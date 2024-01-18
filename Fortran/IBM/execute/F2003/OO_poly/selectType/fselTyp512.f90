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
!*  DATE                       : 04/27/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : select type (use internal IO for converting
!                               poly-data between character and derved types)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) i, j
        character(20) :: name
    end type

    type, extends(base) :: child
        real(8) val
    end type

    interface makeData
        module procedure mapObj2String
    end interface

    contains

    !! this function maps everything into a string
    class (*) function mapObj2String (b)
        allocatable mapObj2String
        class (base), intent(in) :: b

        character(200) c1

        select type (b)
            type is (base)
                write (c1, '(a10,2i20,a25)') 'base:', b
            type is (child)
                write (c1, '(a10,2i20,a25,g25.8)') 'child:', b
            class default
                error stop 10_4
        end select

        allocate (mapObj2String, source=c1)
    end function
end module

program fselTyp512
use m
    class (base), allocatable :: b11, b22
    logical(4) precision_r8

    call translate (b11, makeData(base(1, 10, 'xlf test')))

    call translate (b22, makeData(child(2, 20, 'xlftest 101', 1.5e-3_8)))

    if ((.not. allocated (b11)) .or. (.not. allocated(b22))) error stop 9_4

    !! verify the results
    select type (b11)
        type is (base)
            if ((b11%i /= 1) .or. (b11%j /= 10) .or. (b11%name /= 'xlf test')) &
                    error stop 1_4
        class default
            error stop 2_4
    end select


    select type (b22)
        class is (child)
            if ((b22%i /= 2) .or. (b22%j /= 20) .or. (b22%name /= 'xlftest 101')) &
                error stop 3_4

            if (.not. precision_r8 (b22%val, 1.5e-3_8)) error stop 4_4
        class default
            error stop 5_4
    end select

    contains

    subroutine translate (b1, x)
        class (base), allocatable, intent(out) :: b1
        class(*), intent (in) :: x

        character(10) typeID

        integer(8) i, j
        character(20) :: name
        real(8) val

        select type (x)
            class is (base)
                error stop 11_4
            type is (character(*))
                read (x(:10), *) typeID

                if (typeID == 'base:') then
                    read (x(11:), '(2i20,a25)') i,j,name

                    allocate (b1, source=base(i,j,name))
                else if (typeID == 'child:') then
                    read (x(11:), '(2i20,a25,g25.8)') i,j,name, val
                    allocate (b1, source=child(i,j,name, val))
                else
                    error stop 12_4
                end if

            class default
                error stop 13_4
        end select
    end subroutine
end
