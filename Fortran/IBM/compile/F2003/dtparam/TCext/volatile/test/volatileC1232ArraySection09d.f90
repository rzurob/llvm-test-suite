! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/volatile/test/volatileC1232ArraySection09d.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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

          type base(k1)    ! (4)
            integer, kind :: k1
            sequence
            logical(k1)      b
          end type base

          type child1(k2)    ! (4)
            integer, kind  :: k2
            sequence
            type(base(k2)) :: c1(500)
          end type

          type child2(k3)    ! (4)
             integer, kind    :: k3
             sequence
             type(child1(k3))    c2(100,100)
          end type

          type(child2(4)) y

          y%c2(50:100,10)%c1(123)%b = .true.

          call arraySectionVolatile(y%c2(50:100,10)%c1(123)%b)

  end program volatileC1232ArraySection09d

  subroutine arraySectionVolatile(z)
       logical, VOLATILE:: z(*)
  end subroutine arraySectionVolatile
