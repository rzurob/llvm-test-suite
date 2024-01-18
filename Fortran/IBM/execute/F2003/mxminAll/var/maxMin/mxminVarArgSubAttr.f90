!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with argument with value attribute
!*
!* ===================================================================

  program mxminVarArgSubAttr 

    interface
       subroutine sub1(c1arg, c2arg)
           character*3, value :: c1arg, c2arg
       end subroutine
    end interface

    character*3 x, y, z
    x = "ddd"
    y = "bbb"
    z="ccc"

    call sub1(max(x, y), min(x,y))

    call sub1(max(x, y, z), min(x, y, z))

  end program mxminVarArgSubAttr 

  subroutine sub1(c1arg, c2arg)
      character*3, value :: c1arg, c2arg
      if(max(c1arg, c2arg) .ne. "ddd") error stop 1_4
  end subroutine
