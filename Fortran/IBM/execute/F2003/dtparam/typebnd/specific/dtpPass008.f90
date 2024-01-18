! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2007
!*
!*  DESCRIPTION                : derived type parameter (specific typebnd)
!                               A test case on a string type that converts
!                               FORTRAN character string to C char array
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module CFstrMod
use ISO_C_BINDING
    type string (n)
        integer, len :: n

        character(n) :: str

        contains

        procedure :: toC => convert2CStr
    end type

    interface string
        module procedure createStrObj
        module procedure createStrFromCstr
    end interface

    character(kind=c_char), bind(c, name='FortString') :: globalString(50)

    contains

    function createStrFromCstr (cStr) result(aStr)
        character(kind=C_CHAR), intent(in) :: cStr(*)

        type(string(:)), allocatable :: aStr

        integer count1

        count1 = 1

        do while (cStr(count1) /= achar(0))
            count1 = count1 + 1
        end do

        count1 = count1 - 1

        allocate (string(count1) :: aStr)

        do i = 1, count1
            aStr%str(i:i) = cStr(i)
        end do
    end function

    function convert2CStr (str) result(aStr)
        class(string(*)), intent(in) :: str

        character(kind=C_char) aStr(str%n+1)

        do i = 1, str%n
            aStr(i) = str%str(i:i)
        end do

        aStr(str%n+1) = achar(0)
    end function

    function createStrObj (c1) result(aStr)
        character(*), intent(in) ::c1

        type(string(c1%len)) aStr

        aStr%str = c1
    end function
end module

program dtpPass008
use CFstrMod

    interface
        subroutine printString (s) bind(c, name='printFORTStr')
            use iso_c_binding
            character(kind=C_CHAR), intent(in) :: s(*)
        end subroutine

        integer(C_INT) function copyStr (s, len) bind(c, name = 'copyStr')
            use iso_c_binding
            character(kind=C_CHAR), intent(in) :: s(*)
            integer, value :: len
        end function
    end interface

    type(string(:)), allocatable :: str

    str = string ("a test on string object")

    if (str%n /= len("a test on string object")) error stop 1_4

    call printString (str%toC())

    if (copyStr("2nd test", len("2nd test")) /= 1) error stop 2_4

    str = string (globalString)

    if (str%n /= 8) error stop 3_4

    call printString (str%toC())
end
