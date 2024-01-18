!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with variable as argument 
!*                               in interlanguage calls with C. 
!* ===================================================================

program mxminlocVarBindC 

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

    character*2 x(2,3), y(10)
    logical m(2,3)
    m = .true.
    m(2,1) = .false.
    x = "aa"
    x(2,1) = "dd"
    y = "aa"
    y(6) = "bb"
    
    call sub1(minloc(x, dim=2, mask=m), maxloc(x,dim=1))
    call sub2(maxloc(y, dim=1), minloc(y, dim=1, mask=.true.))

end program mxminlocVarBindC 
