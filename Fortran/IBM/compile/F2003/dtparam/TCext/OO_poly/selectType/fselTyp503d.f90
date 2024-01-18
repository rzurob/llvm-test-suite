! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/fselTyp503d.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

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
! %POSTCMD: tcomp fselTyp503d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/17/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type construct (typo should be
!                               diagnosed)
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

    type, extends(base) :: child    ! (4,20)
        integer(k1) id
    end type

    contains

    class (base(4,:)) function f (b)
        class (base(4,*)), intent(in) :: b

        allocatable f

        allocate (f, source=b)
    end function
end module

program fselTyp503d
use m
    class (base(4,:)), allocatable :: b

    allocate (child(4,20)::b)

    select type (x => f1(b))        !<-- typo
        type is (base(4,*))
        type is (child(4,*))
    end select
end
