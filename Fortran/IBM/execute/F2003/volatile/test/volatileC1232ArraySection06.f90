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
!*  DESCRIPTION                : functional TC for  C1232
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!* ===================================================================

  program volatileC1232ArraySection06

    complex y(16)

    call arraySectionVolatile(y(4:9))        !actual argument is array section

    contains
    subroutine arraySectionVolatile(x)

      complex, VOLATILE::x(:)                 !dummy argument is assumed size
                                              !array
     end subroutine arraySectionVolatile 

  end program volatileC1232ArraySection06
