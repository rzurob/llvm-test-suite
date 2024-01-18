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
!*  DESCRIPTION                : MAX*/MIN* with adjustable array 
!* ===================================================================

program mxminMisc01 

   character*3  x(2,4)

   x = 'aaa'

   call sub1(2, min(x, 'zzz', 'ccc'))

   contains

        subroutine sub1(x, arg)
             integer     x , v(4)
             character*3 arg(1*x, 2*x)
             if(any(arg .ne. 'aaa'))then 
                 error stop 1_4
             endif 
             if(any(maxval(arg, dim=1, mask=.true.) .ne. 'aaa')) then
                  error stop 2_4
             endif
             v = maxloc(arg, dim=1)
             if(any(v .ne. 1)) then
                  error stop 3_4
             endif
        end subroutine

end program mxminMisc01 


