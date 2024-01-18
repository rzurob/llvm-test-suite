!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with where and forall construct
!*                               with its argument is named constant.
!* ===================================================================

  program mxminvalForallWhere

     character*3 x(2,3,5,6,7), y(2,3,5,6,7), v1(2,3,5,7), v2(2,3,5,7)
     integer v(4)

     parameter(x = "_%]", y="gbx")

     where (maxval(x, dim=4) >  minval(x,dim=4, mask=.true.) )
         v1 = maxval(y,dim=4)
     elsewhere
         v1 = minval(y, dim=4, mask=.true.)
     end where

     v = shape(maxval(x, dim=4))

     if(v(1) .ne. 2 .or. v(2) .ne. 3 .or. v(3) .ne. 5 .or. v(4) .ne. 7) then
           error stop 1_4
     endif

     if(any(v1 .ne. "gbx")) then
          error stop 2_4
     endif

     forall(i = 1:7, v1(2,3,5,i) > minval(x))

        where (v2(:,:,:,i) >  minval(x, mask=.true.) )
            v2(:,:,:,i) =  maxval(y)
        elsewhere
            v2(:,:,:,i) = minval(y)
        end where

     end forall

     if(any(v2 .ne. "gbx")) error stop 3_4

  end program mxminvalForallWhere

