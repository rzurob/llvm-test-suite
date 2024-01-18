!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with literal as argument
!*                               in interlanguage calls with C.
!* ===================================================================

program mxminlocBindC

    use  ISO_C_BINDING

    interface
      subroutine sub1(a,b) bind(c)
         use ISO_C_BINDING
         integer(C_INT) :: a(2), b(3)
      end subroutine
      subroutine sub2(x, y) bind(c)
         use ISO_C_BINDING
         integer(C_INT), value :: x, y
      end subroutine
    end interface

    logical m(2,3)
    m = .true.
    m(2,1) = .false.

    call sub1(minloc(reshape((/"aa", "dd", "aa", "ff", "cc", "hh"/), (/2,3/)), dim=2, mask=m), maxloc(reshape((/"aa", "dd", "aa", "ff", "cc", "hh"/), (/2,3/)),dim=1))
    call sub2(maxloc((/"dv", "jk", "sf"/), dim=1), minloc((/"dv", "jk", "sf"/), dim=1, mask=.true.))

end program mxminlocBindC
