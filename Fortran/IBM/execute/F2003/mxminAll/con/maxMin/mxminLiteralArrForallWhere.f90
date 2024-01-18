!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with where and forall construct
!*                               with/without optional argument and argument
!*                               is array section
!*                               MAX/MIN argument is literal
!* ===================================================================

  program mxminLiteralArrForallWhere

     character*3 v1(3), v2(3,5)

     where (max((/"_%]","_%]","_%]"/), (/"gbx","gbx","gbx"/)) >  min((/"_%]","_%]","_%]"/), (/"gbx","gbx","gbx"/)) )
         v1 = max((/"_%]","_%]","_%]"/), (/"gbx","gbx","gbx"/))
     elsewhere
         v1 = min((/"_%]","_%]","_%]"/), (/"gbx","gbx","gbx"/))
     end where

     if(any(v1 .ne. "gbx")) then
          error stop 1_4
     endif

     v2 = max("a", "b")

     forall(i = 1:3, v1(i) > min("a", "b", "a"))

        where (v2(i,:) >  min("a", "b", max("a", "b")) )
            v2(i,:) =  max("a", min("a","b"), "b")
        elsewhere
            v2(i, :) = min("a", min("a","b"), "b")
        end where

     end forall

     if(any(v2 .ne. "b")) then
           error stop 2_4
     endif

  end program mxminLiteralArrForallWhere

