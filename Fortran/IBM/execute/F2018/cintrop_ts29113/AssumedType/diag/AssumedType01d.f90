!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnostic using -qattr and -qattr=full
!*                               to verify the attribute information in
!*                               the listing file
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890

use, intrinsic :: iso_c_binding
implicit none
integer(c_int)         :: i
integer   :: j

interface
   subroutine c_sub(a) BIND(c)
      implicit none
      type(*) :: a
   end subroutine c_sub
end interface

i = 4
call c_sub(i)
if ( i .ne. 99 ) error stop 10
call sub_tgt(j)
call sub_opt(j)
call sub_vlt(j)

contains
   subroutine sub_tgt(b)
      type(*), target :: b
   end subroutine sub_tgt

   subroutine sub_opt(c)
      type(*), optional :: c
   end subroutine sub_opt

   subroutine sub_vlt(d)
      type(*), volatile :: d
   end subroutine sub_vlt
end
