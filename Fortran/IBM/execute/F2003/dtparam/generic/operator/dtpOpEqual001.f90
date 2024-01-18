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
!*  DATE                       : 04/12/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               type-bound generic operator: test the equal
!                               operator for a string type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type string (n)
        integer, len :: n

        character(n) :: val

        contains

        procedure :: equalChar => stringEqualChar
        procedure :: equalStr => stringEqualStr
        generic :: operator(==) => equalChar, equalStr
    end type

    type person
        type(string(30)) :: lName
        type(string(100)) :: fName
        type(string(:)), allocatable :: address

        contains

        procedure :: equal => samePerson
        generic :: operator(==) => equal
    end type

    contains

    logical function stringEqualChar (str, c)
        class(string(*)), intent(in) :: str
        character(*), intent(in) :: c

        stringEqualChar = str%val == c
    end function

    logical function stringEqualStr (str1, str2)
        class(string(*)), intent(in) :: str1, str2

        stringEqualStr = str1%val == str2%val
    end function

    logical function samePerson (p1, p2)
        class(person), intent(in) :: p1, p2

        samePerson = (p1%lName == p2%lName) .and. &
            (p1%fName == p2%fName)
    end function
end module

program dtpOpEqual001
use m
    type(person) :: p1, p2
    pointer p2(:,:)

    !! use structure constructor to assign p1 value
    p1 = person(string(30)('Nice'), string(100)('Guy'), &
        string(50)("8200 Warden Avenue"))

    allocate (p2(2,2))

    !! p2(1,1): use intrinsic assignment for derived types & RHS is structConstr
    p2(1,1)%lName = string(30) ('Nice')
    p2(1,1)%fName = string(100) ('Guy')
    p2(1,1)%address = string(20) ('Somewhere in Canada')

    !! p2(1,2): use derived type's ultimate component
    p2(1,2)%lName%val = 'Smith'
    p2(1,2)%fName%val = 'John'
    allocate (string(40):: p2(1,2)%address)

    p2(1,2)%address%val = 'Markham, Ontario'

    !! p2(2,1): use intrinsic assgn for derived type & RHS is named variables
    p2(2,1)%lName = p2(1,2)%lName
    p2(2,1)%fName = p2(1,1)%fName
    p2(2,1)%address = p2(1,2)%address

    !! p2(2,2) assgned directly from p1
    p2(2,2) = p1

    deallocate (p2(2,2)%address)

    !! now validate the values
    if (.not. (p1 == p2(1,1))) error stop 1_4

    if (.not. (p1%address == '8200 Warden Avenue')) error stop 2_4

    if (.not. (p1%lName == string(5)('Nice'))) error stop 3_4

    if (.not. (p1%fName == string(15)('Guy'))) error stop 4_4

    if (p2(1,1)%address%val /= 'Somewhere in Canada') error stop 5_4

    if (.not. (p2(2,2) == p2(1,1))) error stop 6_4

    if (allocated(p2(2,2)%address)) error stop 7_4

    if (.not. (p2(1,2) == person(string(30)('Smith'), string(100)('John'), &
        string(20)('Markham, Ontario')))) error stop 8_4

    if (.not. (p2(2,1)%lName == 'Smith')) error stop 9_4
    if (.not. (p2(2,1)%fName == 'Guy')) error stop 10_4

    if (.not. (p2(2,1)%address == p2(1,2)%address)) error stop 11_4
end
