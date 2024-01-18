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
!*  DESCRIPTION                : MAX/MIN with literal as 
!*                               argument to intrinc procedures without
!*                               optional argument
!*                               
!*                               character functions are elemental functions
!* ===================================================================

  program mxminLiteralArrArgIntrin1 
    
     ! array reduction function
      if (any(maxval(max(reshape((/"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6)/), (/2,3/)), reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/))), dim=1, mask = .true.) .ne. "exl")) then
              error stop 1_4
      endif
 
      if(count(min(reshape((/"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6)/), (/2,3/)), reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/))) .eq. "IBM") .ne. 6) then
               error stop 2_4
      endif 

     ! array construction function
     
       if(any(cshift(max(reshape((/"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6),"IamIBM"(4:6)/), (/2,3/)), reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/))), shift=(/-1, -1, -1/), dim=1) .ne. "exl")) then
              error stop 3_4
       endif
 
       if(any(shape(pack(max(reshape((/"IBM","IBM","IBM","IBM","IBM","IBM"/), (/2,3/)), reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/))), mask=.true.)) .ne. 6)) then
               error stop 4_4
       endif

     ! array location function

       if(any(minloc(max(reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/)), reshape((/"IBM","IBM","IBM","IBM","IBM","IBM"/), (/2,3/))),dim=1, mask=.true.) .ne. 1)) then
              error stop 5_4
       endif

     ! array inquiry function
 
       if(size(max(reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/)), reshape((/"IBM","IBM","IBM","IBM","IBM","IBM"/), (/2,3/)))) .ne. 6) then
             error stop 6_4
       endif

     ! character function

       if(any(LLE(min(reshape((/"IBM","IBM","IBM","IBM","IBM","IBM"/), (/2,3/)),reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/))), min(reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/)), reshape((/"IBM","IBM","IBM","IBM","IBM","IBM"/), (/2,3/)))) .neqv. .true.)) then
             error stop 7_4
       endif  

       if(any(max(min(reshape((/"IBM","IBM","IBM","IBM","IBM","IBM"/), (/2,3/)), reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/))), reshape((/"exl","exl","exl","exl","exl","exl"/), (/2,3/)) ) .ne. "exl")) then
             error stop 8_4
       endif

  end program mxminLiteralArrArgIntrin1 

