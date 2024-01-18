! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/misc/fintrMisc002.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fintrMisc002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items
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

    end type

    type, extends(base) :: child(k2,n2,k3)    ! (4,20,1,10,8)
        integer, kind             :: k2,k3
        integer, len              :: n2
        character(kind=k2,len=n2) :: name
        integer(k3)                  id
    end type
end module

program fintrMisc002
use m
    class (base(4,:)), allocatable :: b1
    type(child(4,20,1,10,8)) :: c1

    allocate (b1, source=child(4,20,1,10,8)('xlftest', 1))

    if (sizeof(b1) /= sizeof (c1)) error stop 1_4
end

