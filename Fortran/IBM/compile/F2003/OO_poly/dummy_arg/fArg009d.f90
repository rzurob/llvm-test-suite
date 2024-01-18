! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (C529; VALUE attribute can
!                               NOT be specified for a dummy-procedure)
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

    contains

    subroutine abc (b)
        value b
        interface
            function b()
            end function
        end interface
    end subroutine
end module

program fArg009d
end
