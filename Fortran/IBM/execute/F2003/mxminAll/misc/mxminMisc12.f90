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
!*  DESCRIPTION                : MAX*/MIN* with assumed-size array 
!* ===================================================================

program mxminMisc12 

   character*3  x(2,3), y(3,4,5,6)

   x = 'aaa'

   x(2,3) = 'bbb'

   y = 'ddd'

   call sub1(max(x, 'zzz', 'ccc'))

   call sub2(minval(y, dim=1))

   contains

        subroutine sub1(arg)
          character*3 arg(2,*)
          integer v(2)
          if(any(arg(1:2, 1:3) .ne. 'zzz')) then
              error stop 1_4
          endif
          if(maxval(arg(1:2, 1:3)) .ne. 'zzz') then
              error stop 2_4
          endif
          v = maxloc(arg(1:2, 1:3))
          if(v(1) .ne. 1 .or. v(2) .ne. 1) then
              error stop 3_4
          endif

        end subroutine

        subroutine sub2(arg)
          character*3 arg(*)
          if(any(arg(1:120) .ne. 'ddd')) then
              error stop 4_4
          endif
        end subroutine

end program mxminMisc12 


