! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet004d2.f
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
! %POSTCMD: tcomp ffuncRet004d2.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return (diagnostic that test IO can't
!                               be invoked for polymorphic function return
!                               results without DTIO)
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
        integer(k1)      i
    end type

    type, extends(base) :: child    ! (4)
        real(k1) r
    end type

    contains

    class (base(4)) function genData (i, r)
        integer, intent(in) :: i
        real, intent(in), optional :: r

        allocatable genData

        if (present(r)) then
            allocate (genData, source=child(4)(i, r))
        else
            allocate (genData, source=base(4)(i))
        end if
    end function
end module

program ffuncRet004d2
use m
    print *, genData (100, 1.9)     !<-- illegal
end
