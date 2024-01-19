!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : INTENT, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C526
!*
!*   C526: if the volatile attribute is specified, the PARAMETER, EXTERNAL
!*         ,INTRINSIC OR INTENT(IN) shall not be specified.
!* ===================================================================

  program volatileC526Intent01

     integer a

     a = 8

     call sub(a)

     contains
        subroutine sub(b)
          integer, intent(in):: b
          VOLATILE b
        end subroutine

  end program volatileC526Intent01

