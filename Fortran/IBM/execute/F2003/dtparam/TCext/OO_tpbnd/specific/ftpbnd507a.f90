! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_tpbnd/specific/ftpbnd507a.f
! opt variations: -qnok -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd507a.f
! %VERIFY: ftpbnd507a.out:ftpbnd507a.vf
! %STDIN:
! %STDOUT: ftpbnd507a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (nopass binding with
!*                               dummy-args; called by an array)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: typeID => baseID
        procedure, nopass, non_overridable :: typeTell
    end type

    private printBase, baseID, typeTell

    contains

    integer*4 function baseID ()
        baseID = 1
    end function

    integer*4 function typeTell (b)
        class (base(4,*)), intent(in) :: b

        typeTell = b%typeID()
    end function
end module

program ftpbnd507a
use m
    type (base(4,20)) :: b1, b2(10)

    print *, b1%typeTell (b2(3))
    print *, b2%typeTell (b1)
end
