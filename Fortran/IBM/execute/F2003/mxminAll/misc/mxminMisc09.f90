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
!*  DESCRIPTION                : MAX*/MIN* with assumed length char. array.
!*
!*
!* ===================================================================

program mxminMisc09 

    interface
         function fun1(str, n)
             character(*), dimension(*) :: str
             character(n), dimension(n) :: fun1
         end function
         subroutine sub1(str, n)
             character(*), dimension(*) :: str
             character(n), dimension(n) :: ver 
         end subroutine 

    end interface

    character(3), dimension(3) :: str1, str2

    data (str1(i), i=1, 3) /3*'aaa'/
    data (str2(i), i=1, 3) /3*'zzz'/

    if( any(fun1(max(str1, str2, str1), 3) .ne. 'zzz')) then
          error stop 1_4
    endif

    call sub1(min(str1, str2), 3)

end program mxminMisc09 

    function fun1(str, n)
         character(*), dimension(*) :: str
         character(n), dimension(n) :: fun1
         do i = 1, n
             fun1(i) = str(i)
         end do
    end function

    subroutine sub1(str, n)
         character(*), dimension(*) :: str
         character(n), dimension(n) :: ver 
         do i = 1, n
             ver(i) = str(i)
        end do
        if(any(ver .ne. 'aaa')) then
           error stop 2_4
        endif
    end subroutine 


