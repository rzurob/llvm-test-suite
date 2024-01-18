!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod; $KILL $COMPILER -o falloc501d2 $OPTIONS $TR_SRC/falloc501d2.f
! %COMPOPTS: -qfree=f90 -C -qsigtrap=xl__trce
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: ./falloc501d2 2> falloc501d2.out; grep SIGTRAP falloc501d2.out && rm -f falloc501d2.out falloc501d2
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (run-time array bounds checking with
!                               -C)
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
    end type

    type, extends(base) :: child
        character(20) :: name
    end type
end module

program falloc501d2
use m
    class (base), allocatable :: b1(:)
    type (child) :: c1 (10)
    integer(4) size1

    size1 = 5
    c1%id = (/1,2,3,4,5,6,7,8,9,10/)
    c1%name = 'c1'

    allocate (b1(size1), source=c1)

end
