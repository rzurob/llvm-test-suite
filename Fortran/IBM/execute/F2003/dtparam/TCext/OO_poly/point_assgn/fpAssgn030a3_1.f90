! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn030a3_1.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn030a3_1.f
! %VERIFY: fpAssgn030a3_1.out:fpAssgn030a3_1.vf
! %STDIN:
! %STDOUT: fpAssgn030a3_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ASSOCIATE construct (array usage inside
!*                               ASSOCIATE construct; associate-name is array)
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
        integer, kind :: k1
        integer(k1)   :: id
    end type
end module

program fpAssgn030a3_1
use m
    type (base(4)) :: c(2:10)

    associate (x => c(::2))
        x%id = 10

        print *, x
    end associate
end
