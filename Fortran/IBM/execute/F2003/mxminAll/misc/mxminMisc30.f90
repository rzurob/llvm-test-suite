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
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               dummy procedure with variable length dummy
!*                               argument.
!* ===================================================================

program mxminMisc30

      procedure(), pointer :: pp1
      external ext_sub
      pp1 => ext_sub
      call sub(pp1)
      contains
        subroutine sub(dummy_pp1)
          character*3 dummy1
          character*8 dummy2
          character*5 dummy3(10)
          procedure(), pointer :: dummy_pp1
          dummy1 = "abc"
          dummy2 = "xyzabcdf"
          dummy3 = "iamhi"
          call dummy_pp1(max(dummy1, dummy2), minval(dummy3,dim=1))
        end subroutine

end program mxminMisc30

      subroutine ext_sub(arg1, arg2)
        character(*)::arg1, arg2
        if(len(min(arg1 , arg2)) .ne. 8) error stop 1_4
        if(min(arg1, arg2) .ne. "iamhi   ") error stop 2_4
      end subroutine


