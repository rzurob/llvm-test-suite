! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodeferredlp /tstdev/OO_poly/class/fclass008d.f
! opt variations: -qnock -ql -qdeferredlp

! SCCS ID Information
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
! %POSTCMD: dcomp fclass008d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (poly-entities can be in the
!                                array constructor; but they shall have the same
!                                declared types)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program fclass008d
use m
    class (base(4)), allocatable :: b1, b2(:)

    class (child(4,1,20)), allocatable :: c1

    allocate (child(4,1,20):: b1, b2(2))

    allocate (c1)

    print *, shape (reshape ((/b1, b2, c1/), (/2,2/)))
end
