! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-19
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : this is a diagnostic case
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

    real, save :: arr(20)[*]

    call foo (arr(::2)) !<-- this can't happen

    contains

    subroutine foo (x)
        real, intent(inout) :: x(10)[*]
    end subroutine
    end
