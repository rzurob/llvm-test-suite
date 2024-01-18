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
!*  DATE                       : 01/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Same as dtparamCompAccess004 except
!                               involving BIND(C) features to do a conversion
!                               from lower case to upper case for a string.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
use iso_c_binding

    type string (n)
        integer, len :: n

        private

        character(kind=c_char) :: c_str(n)

        contains

        procedure :: data => genString
        procedure :: find => substringOf
        procedure :: toupper => convert2UpperCase
    end type

    interface
        subroutine convertArray (c, i) bind(c, name='convertArray')
        use iso_c_binding
            character(kind=c_char), intent(inout) :: c(*)
            integer(c_int), intent(in) :: i
        end subroutine
    end interface

    interface string
        module procedure produceString
    end interface

    contains

    subroutine convert2UpperCase (s)
        class(string(*)), intent(inout) :: s

        call convertArray (s%c_str, int(s%n, c_int))
    end subroutine

    function produceString (s)
        character(*), intent(in) :: s
        type (string(len(s))) produceString

        do i = 1, len(s)
            produceString%c_str(i) = s(i:i)
        end do
    end function

    function genString (s)
        class(string(*)), intent(in) :: s
        character(s%n) genString

        do i = 1, s%n
            genString(i:i) = s%c_str(i)
        end do
    end function

    integer function substringOf (s, s1)
        class(string(*)), intent(in) :: s, s1

        character(len = s%n) fullStr
        character(len = s1%n) subStr

        fullStr = s%data()
        subStr = s1%data()

        substringOf = index(fullStr, subStr)
    end function
end module

program dtparamCompAccess004a
use m
    class(string(:)), allocatable :: s1

    allocate (s1, source=string('ibm xl fortran compiler'))

    print *, s1%data()

    call s1%toupper()

    if (s1%find(string('FORTRAN')) /= 8) error stop 1_4

    print *, s1%data()
end
