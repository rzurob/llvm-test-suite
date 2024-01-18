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
!*  DATE                       : 09/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               An array case of definedAssgn006.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type number
        integer, allocatable :: id

        contains

        procedure :: assgnID
        generic :: assignment(=) => assgnID
    end type

    type string
        character(:), allocatable :: str

        contains

        procedure :: assgnStr
        generic :: assignment(=) => assgnStr
        procedure :: concatString
        generic :: operator(//) => concatString
    end type

    contains

    elemental subroutine assgnID (a1, a2)
        class(number), intent(out) :: a1
        class(number), intent(in) :: a2

        if (allocated(a2%id)) a1%id = a2%id + 10000
    end subroutine

    elemental subroutine assgnStr (b1, b2)
        class(string), intent(out) :: b1
        class(string), intent(in) :: b2

        if (allocated(b2%str)) b1%str = ' ' // b2%str
    end subroutine

    elemental type(string) function concatString (s, c)
        class(string), intent(in) :: s
        character(*), intent(in) :: c

        if (allocated(s%str)) then
            concatString%str = s%str // c
        else
            concatString%str = c
        end if
    end function
end module

module n
use m
    type employee
        type(number) :: ID
        type(string) :: name(2)
        type(string), allocatable :: address(:)
    end type
end module

program definedAssgn006a
use n
    type(employee), allocatable :: p1(:)
    type(number) num1
    type(string) name(2)

    num1%id = 1

    name(1)%str = 'Mr'
    name(2)%str = 'XYZ'


    p1 = (/(employee(number(i), name // achar(65+mod(i,26)), &
        (/string('8200 Warden Ave.'), string('Markham')/)), i=1,100)/)

    if (.not. allocated(p1)) error stop 1_4

    if ((lbound(p1, 1) /= 1) .or. (ubound(p1, 1) /= 100)) error stop 2_4

    do i = 1, 100
        if ((.not. allocated(p1(i)%ID%id)) .or. &
            (.not. allocated(p1(i)%name(1)%str)) .or. &
            (.not. allocated(p1(i)%name(2)%str)) .or. &
            (.not. allocated(p1(i)%address))) error stop 3_4

        if (p1(i)%ID%id /= 10000+i) error stop 4_4

        if ((p1(i)%name(1)%str /= ' Mr'//achar(65+mod(i,26))) .or. &
            (p1(i)%name(2)%str /= ' XYZ'//achar(65+mod(i,26)))) error stop 5_4

        if ((lbound(p1(i)%address, 1) /= 1) .or. &
            (ubound(p1(i)%address, 1) /= 2)) error stop 6_4

        if ((p1(i)%address(1)%str /= ' 8200 Warden Ave.') .or. &
            (p1(i)%address(2)%str /= ' Markham')) error stop 7_4
    end do
end
