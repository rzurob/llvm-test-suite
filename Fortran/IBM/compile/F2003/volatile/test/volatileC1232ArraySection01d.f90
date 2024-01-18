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
!*  DESCRIPTION                : diagnostic TC for  C1232
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!* ===================================================================

  program volatileC1232ArraySection01d 

    interface
       subroutine arraySectionVolatile(x)
         integer, VOLATILE :: x(4)
       end subroutine arraySectionVolatile 
    end interface

    integer y(16)

    y = 23
    call arraySectionVolatile(y((/2,3,3,6/))) ! array section is
                                              ! vector subscripts
  end program volatileC1232ArraySection01d

  subroutine arraySectionVolatile(x)

    integer, VOLATILE ::x(4)                  ! dummy argument is
    print *, x                                ! explicit array 
    
  end subroutine arraySectionVolatile 
