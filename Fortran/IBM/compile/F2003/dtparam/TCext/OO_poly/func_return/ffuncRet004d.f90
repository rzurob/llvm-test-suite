! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet004d.f
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
! %POSTCMD: dcomp ffuncRet004d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (type-compatibility for
!                               the pointer assignment)
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
        integer(k1) :: id
    end type

    contains

    class (child(4,:)) function produceChildPtr (c)
        class (child(4,*)), intent(in) :: c

        pointer produceChildPtr

        allocate (produceChildPtr, source=c)
    end function
end module

program ffuncRet004d
use m
    type (base(4,:)), pointer :: b

    b => produceChildPtr (child(4,20)(1))
end
