! GB DTP extension using:
! ftcx_dtp -qck -ql -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn024a6.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn024a6.f
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
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (this is a truly a test
!*                               on the fact that a non-poly pointer assigned
!*                               to poly-pointer, the associated target is an
!*                               ancestor component of the target if the dynamic
!*                               types are different)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure, nopass :: typeID => baseTypeID
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure, nopass :: typeID => childTypeID
    end type

    contains

    integer*4 function baseTypeID ()
        baseTypeID = 1
    end function

    integer*4 function childTypeID ()
        childTypeID = 2
    end function
end module

program fpAssgn024a6
use m
    type (child(20,4,1)), pointer :: c1
    class (base(:,4)), pointer :: b1, b2
    type (base(:,4)), pointer :: b3

    allocate (c1)

    c1 = child(20,4,1) (10, 'c1')

    b1 => c1

    b1 => b1

    if (b1%typeID() /= 2) error stop 1_4

    b3 => b1

    b2 => b3

    if (b2%typeID() /= 1) error stop 2_4

    b1 => b3

    if (b1%typeID() /= 1) error stop 3_4

    deallocate (c1)
end
