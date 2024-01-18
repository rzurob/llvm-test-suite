!#######################################################################
!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/run.sh   fxmdvf01  cxmdvf01
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxmdvf01.f
!*
!*  DATE                       : Sep 2,2002
!*
!*  PRIMARY FUNCTIONS TESTED   :test integer variables with bind(c)
!*                              attribute/statement with various
!*                              kind type parameter.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :
!*                              Pass data between a C variable with external
!*				linkage and Fortran variable has the bind(c)
!*				attribute.
!*                              Verify the result of data passing.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  09/02/03    KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
  USE ISO_C_BINDING

  integer(C_SHORT), bind(c) :: x = 10

  CONTAINS
    function assert_eq2(n1,n2)
      INTEGER(C_SHORT), INTENT(IN) :: n1,n2
      LOGICAL assert_eq2
      if (n1 .NE. n2) then
      assert_eq2=.FALSE.
      else
      assert_eq2=.TRUE.
      endif
      return
    END function assert_eq2

end module

PROGRAM sum

  IMPLICIT NONE
  CALL sub1
END PROGRAM sum

SUBROUTINE sub1
   IMPLICIT NONE
   EXTERNAL sum_sq
    CALL sum_sq
  END SUBROUTINE sub1


  SUBROUTINE sum_sq

    use mod
    integer(C_SHORT)::y
    logical::res
    call csub()
    print *, x
    y = 100

    res= assert_eq2(x,y)
    if ( res .eqv. .FALSE.) then
    error stop 220
    endif
  END SUBROUTINE sum_sq
