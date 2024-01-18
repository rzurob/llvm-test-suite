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
!*                               subprogram with argument keyword
!*
!*
!* ===================================================================

program mxminMisc07 

   interface
       subroutine sub1(x, y, z)
           character*4 x, y
           integer z
       end subroutine
   end interface       

   character*4 a, b(3)
   integer     c

   a = "dddd"
   b = "hhhh"

   call sub1(y=maxval(b), z=maxloc(b, dim=1), x=max(a, "zzzz"))

end program mxminMisc07 

    subroutine sub1(x, y,z)
        character*4 x, y
        integer z
        if(x .ne. "zzzz") then
           error stop 1_4
        endif
        if(y .ne. "hhhh") then
           error stop 2_4
        endif
        if(z .ne. 1) then
           error stop 3_4
        endif 
    end subroutine

