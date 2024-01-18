!*  ===================================================================
!*
!*  DATE                       : 20/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : assumed-shape array, VOLATILE
!*
!*  DESCRIPTION                : functional TC for  C1232
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!* ===================================================================

  program volatileC1232ArraySection02

    integer y(5,4,3)
    call arraySectionVolatile(y(3:5:1,2,1:2)) ! subcript triplets

   contains

   subroutine arraySectionVolatile(x)         ! internal subroutine

      integer, VOLATILE:: x(:,:)

    end subroutine arraySectionVolatile

  end program volatileC1232ArraySection02

