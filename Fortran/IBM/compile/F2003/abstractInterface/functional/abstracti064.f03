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

  abstract interface
    subroutine t
    end subroutine
  end interface

contains

subroutine t()
  print *, "This is subroutine t()"
end subroutine t

end module m
