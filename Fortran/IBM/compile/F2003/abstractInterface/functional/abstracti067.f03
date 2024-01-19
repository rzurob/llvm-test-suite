!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:
!*          - Procedure statements with abstract interface
!*          - Procedure pointers statements with abstract interface
!*          - Dummy args for external procedures (subroutines and functions)
!*          - OPTIONAL and INTENT attributes for dummy args.
!*          - DT procedure pointer component with pass attr.
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

subroutine p()
  print *, "This is subroutine p()"
end subroutine p

end module m

program abstracti067

  use m, only : t => p

  abstract interface
    subroutine t
    end subroutine
  end interface

  call t()

end

