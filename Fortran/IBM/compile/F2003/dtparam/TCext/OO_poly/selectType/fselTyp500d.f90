! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/fselTyp500d.f
! opt variations: -qnok -qnol -qreuse=none

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
! %POSTCMD: tcomp fselTyp500d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type construct (abstract type can not in
!                               type-is type-guard)
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
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1) id
    end type

end module

program fselTyp500d
use m
    class (*), pointer :: x

    type (child(4,20)), target :: c1

    x => c1

    select type (x)
        type is (base(4,*))       !<-- this is illegal
!            print *, 'bad'

        class is (base(4,*))       !<-- this ougtht to be legal, see defect 293988
            print *, 'good'
    end select
end


