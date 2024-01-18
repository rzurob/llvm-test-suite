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
!*  DESCRIPTION                : common block var. name as argument to 
!*                               MAX*/MIN* 
!*                                
!* ===================================================================

program mxminMisc15 

    character*3 x(3), y(3)
    common /blk/ x, y

    if(any(max(x, y) .ne. 'zzz'))then
          error stop 1_4
    endif

    if(maxval(x, dim=1) .ne. 'aaa') then
         error stop 2_4
    endif

    if(any(maxloc(y) .ne. 1)) then
         error stop 3_4
    endif
 
end program mxminMisc15 

    block data
        character*3  x(3), y(3)
        common /blk/ x, y
        data (x(i), i=1, 3) /3*'aaa'/
        data (y(i), i=1, 3) /3*'zzz'/
    end block data

