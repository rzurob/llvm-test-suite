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
!*  DESCRIPTION                : MAX*/MIN* with where 
!*                                
!* ===================================================================

program mxminMisc27 

    character*3 x(3, 4, 5, 6), y(3,4,5,6)
    integer x_v(4,5,6), y_v(4,5,6)

    logical z(4,5,6)

    x = 'ccc'
 
    y = 'ddd'

    y_v = 11

    where(maxval(x, dim=1, mask=.true.) > "aaa")

     x_v = maxloc(x, dim=1) 

    end where

    if(any(x_v .ne. 1)) then
        error stop 1_4
    endif

    where(maxloc(x, dim=1) > 0 )    y_v = 12

    if(any(y_v .ne. 12)) then
        error stop 2_4
    endif

    where(x < y) 
       x = max(x, y)
    elsewhere
       x = min(x, y)
    end where 

    if(any(x .ne. "ddd")) then
       error stop 3_4
    endif

end program mxminMisc27 


