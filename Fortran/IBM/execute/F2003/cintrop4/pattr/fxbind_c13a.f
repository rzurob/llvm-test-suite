! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan. 1, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test interface bind(c) subroutine
!*                                with c function pointer as argument.
!*                                Subwoutine is implemented in C and
!*                                called from fortran
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program p1
use iso_c_binding
   interface
     subroutine sub(f, arg, ret) bind(c)
       use iso_c_binding
       type(C_FUNPTR) :: f
       integer*4 ret, arg
     end subroutine sub
     integer function f(x) bind(c)
        integer x
     end function f
   end interface

   integer*4 ret, arg /2/
   type(C_FUNPTR) :: fp
   fp = C_FUNLOC(f)

   call sub(fp, arg, ret)

   if (ret .ne. 4) then
     error stop 10
   end if

end program p1

integer function f(x) bind(c)
   integer x

   f = x**2
end function f

