!*  ===================================================================
!*
!*  DATE                       : 20/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : assumed-shape array, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C1232
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!* ===================================================================

  program volatileC1232ArraySection09d

    interface
       subroutine arraySectionVolatile(x)
           logical,VOLATILE:: x(*)           ! dummy argument is assumed
       end subroutine arraySectionVolatile   ! size  array
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

  end program volatileC1232ArraySection09d

  subroutine arraySectionVolatile(z)
       logical, VOLATILE:: z(*)
  end subroutine arraySectionVolatile
