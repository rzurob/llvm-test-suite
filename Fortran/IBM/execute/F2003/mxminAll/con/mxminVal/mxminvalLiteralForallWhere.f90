!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with where and forall construct 
!*                               with its argument is literal. 
!* ===================================================================

  program mxminvalLiteralForallWhere 

     character*3 x(2,3), y(2,3), v1(2), v2(2)
     integer v(2)

     parameter(x = "_%]", y="gbx")

     where (maxval(reshape((/"_%]","aaa","_%]","_%]","_%]","_%]"/), (/2,3/)), dim=2) >=  minval(reshape((/"_%]","_%]","_%]","_%]","_%]","_%]"/), (/2,3/)),dim=2, mask=.true.) )
         v1 = maxval(reshape((/"gbx","gbx","gbx","gbx","gbx","aaa"/), (/2,3/)),dim=2)
     elsewhere
         v1 = minval(reshape((/"gbx","gbx","gbx","gbx","gbx","aaa"/), (/2,3/)), dim=2, mask=.true.)
     end where       

     if(any(shape(maxval(reshape((/"_%]","_%]","_%]","_%]","_%]","aaa"/), (/2,3/)), dim=2)) .ne. 2)) error stop 1_4

     if(any(v1 .ne. "gbx")) then
          error stop 2_4
     endif

     forall(i = 1:2, v1(i) > minval(reshape((/"_%]","_%]","_%]","_%]","_%]","aaa"/), (/2,3/))))

        where (v2(:) >  minval(reshape((/"_%]","_%]","_%]","_%]","_%]","aaa"/), (/2,3/))))
            v2(:) =  maxval(reshape((/"bbb","gbx","gbx","gbx","gbx","gbx"/), (/2,3/)),dim=2) 
        elsewhere
            v2(:) = minval(reshape((/"zzz","gbx","gbx","gbx","gbx","gbx"/), (/2,3/)),dim=2) 
        end where

     end forall

     if(any(v2 .ne. "gbx")) error stop 3_4 

  end program mxminvalLiteralForallWhere  

