! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn023a1.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn023a1.f
! %VERIFY: fpAssgn023a1.out:fpAssgn023a1.vf
! %STDIN:
! %STDOUT: fpAssgn023a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (deallocate unlimited
!                               poly pointer pointed to aother allocated pointer)
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
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

program fpAssgn023a1
use m
    class (*), pointer :: x

    type (base(4)), pointer :: b1

    allocate (b1)

    x => b1

    deallocate (x)
end
