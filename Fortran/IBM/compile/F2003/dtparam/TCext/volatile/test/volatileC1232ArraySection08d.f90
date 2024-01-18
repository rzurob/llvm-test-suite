! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/volatile/test/volatileC1232ArraySection08d.f
! opt variations: -qck -qnok

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

    type dt(k1,n1)    ! (4,10)
        integer, kind :: k1
        integer, len  :: n1
        sequence
        character(n1)    string(5)
    end type dt

    procedure(arraySectionVolatile), pointer:: ptr ! procedure pointer

    interface iface                              ! use module procedure
        procedure ptr
    end interface

    type(dt(4,10)) var

    ptr => arraySectionVolatile

    var%string(:)(1:3) ='abc'
                                                  ! call module subprogram
    call iface(var%string(:)(1:3))                ! substring range

  end program volatileC1232ArraySection08d

  subroutine arraySectionVolatile(x)
       character(3), VOLATILE :: x(*)
  end subroutine arraySectionVolatile   

