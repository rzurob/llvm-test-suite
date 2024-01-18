
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class keyword (unallocated allocatable has a
!*                               dynamic type of its declared type)
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
        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure, nopass :: typeID => childID
    end type

    contains

    integer*4 function baseID()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module

program fclass001
use m

    class (base), allocatable :: b_allo

    if (b_allo%typeID() /= 1) error stop 1_4

    allocate (child:: b_allo)

    if (b_allo%typeID() /= 2) error stop 2_4

    deallocate (b_allo)

    if (b_allo%typeID() /= 1) error stop 3_4
end
