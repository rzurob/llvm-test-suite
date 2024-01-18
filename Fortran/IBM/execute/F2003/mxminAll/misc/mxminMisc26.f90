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
!*  DESCRIPTION                : MAX*/MIN* with value attribute
!*                                
!* ===================================================================

program mxminMisc26  

    character*1 x, y(3), z(3)

    x = "a"

    y = "b"

    z = "c"

    call sub1(x, max("z", "b"))

    if(x .ne. "a") then 
      error stop 1_4
    endif 

    x = "x"

    call sub2(x, maxval(y))

    if(x .ne. "x")then
        error stop 2_4
    endif 

 
end program mxminMisc26 

  subroutine sub1(arg1, arg2)
      character*1 :: arg1,arg2
      value :: arg1, arg2
      arg1 = max(arg1, arg2)
  end subroutine

  subroutine sub2(arg1, arg2)
      character*1, value :: arg1,arg2
      arg1 = arg2
  end subroutine 

