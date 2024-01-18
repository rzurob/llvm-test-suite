!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE attribute, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C527
!*
!*   C526: if the value attribute is specified, the ....VOLATILE attribute
!*         shall not be specified.
!* ===================================================================

  module mod
    implicit none
    type tt
      integer x(2,2)
    end type
  end module

  program volatileC527Value
     use mod
     implicit none
     interface
       subroutine sub(xx)
         use mod
         implicit none
         type(tt), value :: xx
         VOLATILE xx
       end subroutine
     end interface

  end program volatileC527Value

