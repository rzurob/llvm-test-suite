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

  program volatileC526Intent03

     abstract interface
        subroutine sub(b)
          real, intent(in):: b
          VOLATILE b
        end subroutine
     end interface

    real x
    procedure(sub) :: subnew
    procedure(sub), pointer :: p

    x = 3.0
    p => subnew

    call p(x)

  end program volatileC526Intent03

  subroutine subnew(b)
     real, intent(in):: b
     VOLATILE b
  end subroutine
