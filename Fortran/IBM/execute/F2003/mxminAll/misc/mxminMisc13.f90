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
!*  DESCRIPTION                : MAX*/MIN* with automatic array
!* ===================================================================

program mxminMisc13 

   character*2 a(10)

   a = 'aa'

   a(8) = 'zz'

   call sub1(maxloc(a, dim=1))

   contains
        subroutine sub1(arg)
            integer arg, v(2)
            character*6 x(1:arg, 2:6)
            x = "abcdeg"
            x(8, 6) = 'zzzzzz' 
            if(maxval(x) .ne. 'zzzzzz') then
                error stop 2_4
            endif
            v = maxloc(x)
            if(v(1) .ne. 8 .or. v(2) .ne. 5) then
                error stop 3_4
            endif
        end subroutine

end program mxminMisc13 

