!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 20/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : assumed-shape array, VOLATILE
!*
!*
!*  DESCRIPTION                : functional TC for  C1232
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!* ===================================================================

  program volatileC1232ArraySection04

    interface
       subroutine arraySectionVolatile(x)
           logical,VOLATILE:: x(:)          
       end subroutine arraySectionVolatile   
    end interface

          type base
            sequence
            logical b
          end type base 

          type child1
            sequence
            type(base) :: c1(500)
          end type

          type child2
             sequence 
             type(child1) c2(100,100)
          end type

          type(child2) y 

          y%c2(50:100,10)%c1(123)%b = .true.

          call arraySectionVolatile(y%c2(50:100,10)%c1(123)%b)

  end program volatileC1232ArraySection04

  subroutine arraySectionVolatile(z)
       logical, VOLATILE:: z(:)            
  end subroutine arraySectionVolatile
