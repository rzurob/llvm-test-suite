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
!*  DESCRIPTION                : MAXVAL/MINVAL with constant as argument
!*                               with other intrinsics.
!* ===================================================================

  program mxminvalIntrin  
    
     character*3 x(2,3), y(2,3)
     parameter(x = "IBM")
     parameter(y="exl")

     ! array reduction function
      if (any(maxval(max(x, y), dim=1, mask = .true.) .ne. "exl")) then
              error stop 1_4
      endif

      if(count(minval(x,dim=2) .eq. "IBM") .ne. 2) then
               error stop 2_4
      endif 

     ! array construction function

       if(any(cshift(maxval(x,dim=2), shift=-1, dim=1) .ne. "IBM")) then
              error stop 3_4
       endif

       if(any(shape(pack(maxval(x,dim=2), mask=.true.)) .ne. 2)) then
               error stop 4_4
       endif

     ! array inquiry function

       if(size(maxval(x,dim=2)) .ne. 2) then
             error stop 5_4
       endif

     ! character function

       if(any(LLT(maxval(x,dim=2), minval(x,dim=2)) .neqv. .false.)) then
             error stop 6_4
       endif  

       if(any(LGT(maxval(y,dim=1), minval(x,dim=1)) .neqv. .true.)) then
             error stop 7_4
       endif  

       if(any(LGE(maxval(y,dim=2), minval(x, dim=2)) .neqv. .true.)) then
             error stop 8_4
       endif  

       if(any(LLE(minval(x,dim=1,mask=.true.), maxval(y,dim=1,mask=.true.)) .neqv. .true.)) then
             error stop 9_4
       endif  

  end program mxminvalIntrin  

