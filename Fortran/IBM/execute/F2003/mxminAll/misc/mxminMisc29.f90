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
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               intrinsic function -- char and ichar
!*                               
!*                                
!* ===================================================================

program mxminMisc29 

    character*1 x(33)
    x = 'g'

    if(ichar(maxval(x)) .ne. ichar(x(1))) then
        error stop 1_4
    endif

    x(33) = "z"

    if(char(maxloc(x, dim=1)) .ne. "!") then
         error stop 2_4
    endif

    if(ichar(max(x(1), x(33))) .ne. ichar("z")) then
          error stop 3_4
    endif
 
end program mxminMisc29 


