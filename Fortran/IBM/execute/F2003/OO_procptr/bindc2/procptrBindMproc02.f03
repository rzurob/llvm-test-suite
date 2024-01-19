!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TC for C_F_PROCPOINTER:
!*
!*                               function pointer is an array of pointers
!*                               associated with procedure pointer through
!*                               C_F_PROCPOINTER. Procedure pointer and
!*                               external procedure be argument to C_FUNLOC.
!* ===================================================================

module fptr02
   use ISO_C_BINDING
   type(C_FUNPTR), bind(c) :: cptr(2)
end module fptr02

program procptrBindMproc02

   use ISO_C_BINDING

   use fptr02

   interface
       subroutine csub1(x) bind(c)
          import
          integer(C_INT) :: x
       end subroutine csub1
   end interface

   interface
       subroutine csub2(y) bind(c)
          import
          integer(C_INT) :: y
       end subroutine csub2
   end interface

   procedure(csub1), pointer :: cp1 => null()
   procedure(csub2), pointer :: cp2 => null()

   integer(C_INT) :: arg1
   integer(C_INT) :: arg2

   cp2=>csub2

   cptr(1) = C_FUNLOC(csub1)
   if(.not. C_ASSOCIATED(cptr(1))) error stop 1_4
   if(.not. C_ASSOCIATED(cptr(1), C_FUNLOC(csub1))) error stop 2_4

   cptr(2) = C_FUNLOC(cp2)
   if(.not. C_ASSOCIATED(cptr(2))) error stop 3_4
   if(.not. C_ASSOCIATED(cptr(2), C_FUNLOC(cp2))) error stop 4_4

   if(ASSOCIATED(cp1)) error stop 5_4
   call C_F_PROCPOINTER(cptr(1), cp1)
   if(.not. ASSOCIATED(cp1)) error stop 6_4

   arg1 = 1_C_INT
   arg2 = 2_C_INT

   call cp1(arg1)
   call cp2(arg2)

   if(arg1 .ne. 10) error stop 1_4
   if(arg2 .ne. 20) error stop 2_4

end program procptrBindMproc02

