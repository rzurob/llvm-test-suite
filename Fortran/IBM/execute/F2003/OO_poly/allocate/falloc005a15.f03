! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (use of elemental function as
!                               source-expr)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) :: id

        contains

        procedure :: string => baseString
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: string => childString
    end type

    contains

    elemental character(30) function baseString (b)
        class (base), intent(in) :: b

        write (baseString, *) b%id
    end function

    elemental character(30) function childString (b)
        class (child), intent(in) :: b

        write (childString, *) b%id, b%name
    end function
end module

program falloc005a15
use m
    type (child) :: c1 (10)
    character(30), allocatable :: ch1(:), ch2(:)

    character(30) :: c_ver

    c1%id = (/(i, i=1,10)/)
    c1%name = 'c1'

    allocate (ch1(size(c1)), source=c1%string())

    allocate (ch2(3), source=c1(5:7)%base%string())

    !! verify results
    do i = 1, 10
        write (c_ver,*) i, 'c1'

        if (c_ver /= ch1(i)) call zzrc(int(i,4_4))
    end do

    if ((ch2(1) /= ' 5') .or. (ch2(2) /= ' 6') .or. (ch2(3) /= ' 7')) &
            error stop 15

    deallocate (ch1, ch2)
end
