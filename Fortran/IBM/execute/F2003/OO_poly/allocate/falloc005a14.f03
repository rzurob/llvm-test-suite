! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE ((expression) as the source-expr in
!                               the ALLOCATE statement; use array constructor,
!                               array element and array section)
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

    character(30) function baseString (b)
        class (base), intent(in) :: b

        write (baseString, *) b%id
    end function

    character(30) function childString (b)
        class (child), intent(in) :: b

        write (childString, *) b%id, b%name
    end function
end module

program falloc005a14
use m
    real(8), allocatable :: r1(:)

    class (base), allocatable :: b1(:), b2(:)
    class (base), pointer :: b3(:), b4(:)

    type (child) :: c1 (10)

    c1%id = (/(i*10, i=1,10)/)
    c1%name = 'c1'

    !! (array constructor) as source-expr
    allocate (r1(5), source=((/1.0d0, (1.2d1/i, i=2,4), 10.0d0/)))

    allocate (b1(3), source=((/(child(i, 'b1_first2'), i=1,2), &
                    child(10,'b1_last')/)))

    !! (array element) in source-expr
    allocate (b2(2), source = (c1(5)))


    !! (array section) in source-expr
    allocate (b3(3), source=(c1(4:6)%base))


    allocate (b4(2), source=(c1(8:9)))


    if (any (r1 /= (/1.0d0, 6.0d0, 4.0d0, 3.0d0, 1.0d1/))) error stop 1_4

    if (b2(1)%string() /= ' 50 c1') error stop 2_4
    if (b2(2)%string() /= b2(1)%string()) error stop 3_4

    if (b3(1)%string() /= ' 40') error stop 4_4
    if (b3(2)%string() /= ' 50') error stop 5_4
    if (b3(3)%string() /= ' 60') error stop 6_4

    if ((b4(1)%string() /= ' 80 c1') .or. (b4(2)%string() /= ' 90 c1'))  &
            error stop 7_4

    if ((b1(1)%string() /= ' 1 b1_first2') .or. &
        (b1(2)%string() /= ' 2 b1_first2') .or. &
        (b1(3)%string() /= ' 10 b1_last')) error stop 8_4
end