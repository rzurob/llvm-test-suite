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

program abstracti063

  abstract interface
    subroutine t
    end subroutine
  end interface

  interface
    subroutine test1 (p)
      import
      procedure(t) :: p
    end subroutine
  end interface

  call test1 (t)

end

subroutine test1(x)
  abstract interface
    subroutine t
    end subroutine
  end interface

  procedure(t) x

  call x()
end

subroutine t()
  print *, "This is subroutine t()"
end