!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with where and forall construct
!*                               with/without optional argument and argument
!*                               is array section
!* ===================================================================

  program mxminArrayArgForallWhere

     character*3 x(20), y(20), v1(10), v2(10,5) , x1, y1
     parameter(x = "_%]", y="gbx", x1="aaa", y1="bbb")

     where (max(x(1:10), y(11:20)) >  min(x(1:10), y(1:10)) )
         v1 = max(x(1:10), y(1:10))
     elsewhere
         v1 = min(x(1:10), y(1:10))
     end where

     if(any(v1 .ne. "gbx")) then
          error stop 1_4
     endif

     v2 = max(x1, y1)

     forall(i = 1:10, v1(i) > min(x1, y1, x1))

        where (v2(i,:) >  min(x1, y1, max(x1,y1)) )
            v2(i,:) =  max(x1, min(x1,y1), y1)
        elsewhere
            v2(i, :) = min(x1, min(x1,y1), y1)
        end where

     end forall

     if(any(v2 .ne. "bbb")) then
           error stop 2_4
     endif

  end program mxminArrayArgForallWhere

