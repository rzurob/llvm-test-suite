!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 3/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                :  TC for C_F_PROCPOINTER and procedure pointer
!*                                with different prototype. 
!*  (317443)
!* ===================================================================

program procptrBindMproc05d

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNLOC, C_PTR, C_INT, C_FLOAT

   interface
       subroutine csub1(i) bind(c)
         import C_FLOAT
         real(C_FLOAT) :: i
       end subroutine csub1
   end interface

   interface
       subroutine csub2(i) bind(c)
         import C_INT
         integer(C_INT) :: i
       end subroutine csub2
   end interface

   interface
       subroutine csub3(i, j) bind(c)
         import C_INT
         integer(C_INT) :: i, j
       end subroutine csub3
   end interface

   procedure(csub2), pointer :: pbind1

   procedure(csub3), pointer :: pbind2

   call C_F_PROCPOINTER(C_FUNLOC(csub1), pbind1)

   pbind1=>csub1

   call C_F_PROCPOINTER(C_FUNLOC(csub2), pbind2)

   pbind2=>csub2

end program procptrBindMproc05d

