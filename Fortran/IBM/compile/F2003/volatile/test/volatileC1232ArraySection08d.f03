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

  program volatileC1232ArraySection08d

    interface
       subroutine arraySectionVolatile(x)
          character(3), VOLATILE :: x(*)
       end subroutine arraySectionVolatile
    end interface

    type dt
        sequence
        character(10) string(5)
    end type dt

    procedure(arraySectionVolatile), pointer:: ptr ! procedure pointer

    interface iface                              ! use module procedure
        procedure ptr
    end interface

    type(dt) var

    ptr => arraySectionVolatile

    var%string(:)(1:3) ='abc'
                                                  ! call module subprogram
    call iface(var%string(:)(1:3))                ! substring range

  end program volatileC1232ArraySection08d

  subroutine arraySectionVolatile(x)
       character(3), VOLATILE :: x(*)
  end subroutine arraySectionVolatile
