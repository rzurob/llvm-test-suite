!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : INTENT, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C526
!*
!*   C526: if the volatile attribute is specified, the PARAMETER, EXTERNAL
!*         ,INTRINSIC OR INTENT(IN) shall not be specified.
!* ===================================================================

  module m
     contains
        subroutine sub(b)
          type base
             sequence
             complex x
          end type
          type(base), intent(in):: b
          VOLATILE b
        end subroutine
  end module m

  program volatileC526Intent02
    use m

    type base
      sequence
      complex x
    end type
    type(base) :: a
    a%x = (1.0,2.0)

    call sub(a)

  end program volatileC526Intent02

