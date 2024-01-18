!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a13.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE ((expression) as the source-expr in
!                               ALLOCATE statement)
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

    character(50) function baseString (b)
        class (base), intent(in) :: b

        write (baseString, *) b%id
    end function

    character(50) function childString (b)
        class (child), intent(in) :: b

        write (childString, *) b%id, b%name
    end function
end module

program falloc005a13
use m
    class (base), pointer :: b1, b2(:)

    type (child) :: c1 = child (10, 'c1')

    real(8), allocatable :: r1

    character(15), allocatable :: ch1(:)

    logical precision_r8

    allocate (b1, source=(child(1,'b1')))

    allocate (b2(2:3), source=(c1))

    allocate (r1, source = (2.0d0**2))

    allocate (ch1(-100:-99), source=('xlftest'//' team'))

    if (b1%string() /= ' 1 b1') error stop 1_4

    if ((b2(2)%string() /= ' 10 c1') .or. (b2(3)%string() /= ' 10 c1')) &
            error stop 2_4

    if (.not. precision_r8 (r1, 4.0d0)) error stop 3_4

    if (any (ch1 /= 'xlftest team')) error stop 4_4
end
