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

  program volatileC1232ArraySection07d 

    complex y(16)
    integer x, z

    x = 6

    z = arraySectionVolatile(y(4:9), x)     !actual argument is array section

    contains
    integer function arraySectionVolatile(x, y)

      integer y
      complex, VOLATILE::x(1:y)               !dummy argument is adjustable  
      arraySectionVolatile = y+5              !array
     end function arraySectionVolatile 

  end program volatileC1232ArraySection07d
