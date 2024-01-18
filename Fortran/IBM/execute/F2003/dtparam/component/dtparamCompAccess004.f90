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
!*  DATE                       : 01/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: component)
!                               Case: Derived type parameter and private access
!                               for component: character component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type string (n)
        integer, len :: n

        private

        character :: c_str(n)

        contains

        procedure :: index => indexOf
        procedure :: data => genString
        procedure :: find => substringOf
    end type

    interface string
        module procedure produceString
    end interface

    contains

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

    integer function indexOf (s, c)
        class(string(*)), intent(in) :: s
        character, intent(in) :: c

        do i = 1, s%n
            if (c == s%c_str(i)) then
                indexOf = i
                return
            end if
        end do

        indexOf = -1
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

program dtparamCompAccess004
use m
    class(string(:)), allocatable :: s1

    allocate (s1, source=string('ibm xl fortran compiler'))

    print *, s1%data()

    if (s1%find(string('fortran')) /= 8) error stop 1_4

    if (s1%index('o') /= 9) error stop 2_4
end
