! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/volatile/test/volatileC1232ArraySection03.f
! opt variations: -qck -qnok

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

  module m

   contains
       subroutine arraySectionVolatile(x)
          character(3), VOLATILE :: x(:)      ! dummy argument is assumed
       end subroutine arraySectionVolatile    ! shape array

  end module m

  program volatileC1232ArraySection03d

    use m

    type dt(k1,n1)    ! (4,10)
        integer, kind :: k1
        integer, len  :: n1
        sequence
        character(n1)    string(5)
    end type dt

    type(dt(4,10)) var

    var%string(:)(1:3) ='abc'
                                                  ! call module subprogram
    call arraySectionVolatile(var%string(:)(1:3)) ! substring range

  end program volatileC1232ArraySection03d