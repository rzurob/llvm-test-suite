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
!*  PRIMARY FUNCTIONS TESTED   : array section, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  12.4.1.2 
!*     12.4.1.2:
!*        If the actual argument is an array section having vector
!*     subscript, the dummy argumnet is not definable and should not
!*     have, intent(out), intent(inout), volatile, or asynchronous
!*     attributes. 
!* ===================================================================

  program volatileAttrIntent 

    interface
       subroutine arraySectionVolatile(x)
         integer, intent(in) :: x(:)
         VOLATILE x
       end subroutine arraySectionVolatile 
    end interface

    integer y(16)

    y = 23
    call arraySectionVolatile(y((/2,3,3,6/))) ! array section is
                                              ! vector subscripts
  end program volatileAttrIntent 

  subroutine arraySectionVolatile(x)

    integer, intent(in)  ::x(:)               ! dummy argument is
    VOLATILE :: x                             ! with intent(in) 
  end subroutine arraySectionVolatile 
