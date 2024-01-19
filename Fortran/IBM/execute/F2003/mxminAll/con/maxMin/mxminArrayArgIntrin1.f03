!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with named constant as
!*                               argument to intrinc procedures without
!*                               optional argument
!*
!*                               character functions are elemental functions
!* ===================================================================

  program mxminArrayArgIntrin1

     character*3 x(2,3), y(2,3)
     parameter(x = "IBM", y="exl")

     ! array reduction function
      if (any(maxval(max(x, y), dim=1, mask = .true.) .ne. "exl")) then
              error stop 1_4
      endif

      if(count(min(x, y) .eq. "IBM") .ne. 6) then
               error stop 2_4
      endif

     ! array construction function

       if(any(cshift(max(x, y), shift=(/-1, -1, -1/), dim=1) .ne. "exl")) then
              error stop 3_4
       endif

       if(any(shape(pack(max(x, y), mask=.true.)) .ne. 6)) then
               error stop 4_4
       endif

       if(any(pack(min(x, y), mask=.true.) .ne. "IBM")) then
               error stop 5_4
       endif

     ! array location function

       if(any(minloc(max(x, y),dim=1, mask=.true.) .ne. 1)) then
              error stop 6_4
       endif

     ! array inquiry function

       if(size(max(x, y)) .ne. 6) then
             error stop 7_4
       endif

     ! character function

       if(any(LLT(max(x,y), min(y, x)) .neqv. .false.)) then
             error stop 8_4
       endif

       if(any(LGT(max(x,y), min(y, x)) .neqv. .true.)) then
             error stop 9_4
       endif

       if(any(LGE(max(x,y), max(y, x)) .neqv. .true.)) then
             error stop 10_4
       endif

       if(any(LLE(min(x,y), min(y, x)) .neqv. .true.)) then
             error stop 11_4
       endif

       if(any(max(min(x, y), y) .ne. "exl")) then
             error stop 12_4
       endif

  end program mxminArrayArgIntrin1

