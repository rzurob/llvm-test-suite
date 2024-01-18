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
!*  DESCRIPTION                : diagnostic TC for argument with 
!*                               different kind 
!*
!*  (314990)
!* ===================================================================

  program mxminConform02d

     integer    z1(2,4)
     integer(8) z2(2,4)

     parameter(z1 = 98)
     parameter(z2 = 100)

     print *, max(z1, z2)

     print *, min(z2, z2, z1)

  end program mxminConform02d
