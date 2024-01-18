!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 12/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : array section with vector subscript
!*                               VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  12.4.1.2 
!*     12.4.1.2:
!*        If the actual argument is an array section having vector
!*     subscript, the dummy argumnet is not definable and should not
!*     have, intent(out), intent(inout), volatile, or asynchronous
!*     attributes. 
!* ===================================================================

  program volatileOtherAttr03d

    interface
       subroutine arraySectionVolatile(x)
         integer, intent(out) :: x(:)
       end subroutine arraySectionVolatile 
    end interface

    integer y(16)

    y = 23
    call arraySectionVolatile(y((/2,3,3,6/))) ! array section is
                                              ! vector subscripts
  end program volatileOtherAttr03d

  subroutine arraySectionVolatile(x)

    integer, intent(out) ::x(:)               
    VOLATILE::x
  end subroutine arraySectionVolatile 
