!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg013a6.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (TARGET attribute of
!                               dummy-arg and pointer assignment)
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
    end type

    type, extends(base) :: child
        integer*4 :: id
        character*20 :: name
    end type

    type container
        class (base), pointer :: data (:) => null()
    end type

    class (container), allocatable :: co1 (:)

    contains

    subroutine test1 (b)
        class (base), target, intent(in) :: b (10:)

        !! assign the last element in co1 to input
        co1(ubound(co1,1))%data => b
    end subroutine
end module

program fArg013a6
use m
    type (child) :: c1 (4)

    c1 = (/child(1,'c1_1'), child(2,'c1_2'), child(3,'c1_3'), child(4,'c1_4')/)

    allocate (co1(0:5))

    allocate (co1(0)%data(0:3), source=c1)

    call test1 (co1(0)%data)

    if (.not. associated (co1(0)%data, co1(5)%data)) error stop 1_4

    if ((lbound (co1(5)%data, 1) /= 10) .or. (ubound(co1(5)%data, 1) /= 13)) &
            error stop 2_4
end
