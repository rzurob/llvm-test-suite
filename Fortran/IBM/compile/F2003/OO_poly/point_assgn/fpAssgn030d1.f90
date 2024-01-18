!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fpAssgn030d1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (associate-name in
!                               ASSOCIATE construct doe not have pointer or
!                               allocatable attributes)
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
        integer*4 :: id
    end type
end module

program fpAssgn030d1
use m
    class (base), pointer :: b1

    class (base), allocatable :: b2(:)

    allocate (b1)
    allocate (b2(10))

    associate (x => b1, y => b2)
        allocate (x)    !<-- this is illegal
        deallocate (y)  !<-- this is illegal
    end associate
end
