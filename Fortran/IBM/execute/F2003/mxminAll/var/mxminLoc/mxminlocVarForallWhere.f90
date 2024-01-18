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
!*  DESCRIPTION                : MAXLOC/MINLOC with where and forall construct 
!* ===================================================================

  program mxminlocVarForallWhere 

     integer v(2,3,5), v1(5,5)
     character*3 x(2,3, 4,5), y(2,3,4,5), z(4)
     x = "xyz" 
     y="abc"
     z="kjh"
     z(1) = "aaa"

     v1 = 1

     where (maxloc(x,dim=3, mask=.true.) ==  minloc(y,dim=3, mask=.true.))
         v = maxloc(y, dim=3) 
     elsewhere
         v = 0 
     end where       

     if(any(v .ne. 1)) error stop 1_4

     forall(i = 1:5, v(2,3,i) < maxloc(z,dim=1))

        where (v1(i,:) >  minloc(z, dim=1))
            v1(i,:) =  minloc(z, dim=1) 
        elsewhere
            v1(i, :) = maxloc(z, dim=1) 
        end where

     end forall

     if(any(v1 .ne. 2)) error stop 2_4
 
  end program mxminlocVarForallWhere 
