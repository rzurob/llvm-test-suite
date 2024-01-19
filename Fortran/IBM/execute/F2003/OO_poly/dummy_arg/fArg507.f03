! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : arg-association (poly assumed shape dummy-arg,
!*                               the lower bounds starts from 1)
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

program fArg507
    integer*4 i(2:10)

    call abc (i)

    contains

    subroutine abc (x)
        class(*) x (:)

        print *, lbound(x), ubound(x)
    end subroutine
end
