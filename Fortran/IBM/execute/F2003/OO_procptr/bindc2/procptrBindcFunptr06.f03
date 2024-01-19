!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                              function pointer which points to C function with int
!*                              argument and an int return, as actual argument
!*                              passed to interoperable subprogram through procedure
!*                              pointer reference. Use C_F_PROCPOINTER to
!*                              associate procedure pointer with c address of
!*                              procedure.
!* ===================================================================

module fptr06
   use ISO_C_BINDING
   interface
       subroutine csub(i, cptr) bind(c)
          import C_INT, C_FUNPTR
          integer(C_INT) :: i
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr06

program procptrBindcFunptr06

   use ISO_C_BINDING

   use fptr06

   interface
       integer(C_INT) function cfun(i) bind(c)
          import C_INT
          integer(C_INT), value :: i
       end function
   end interface

   integer(C_INT) :: i

   procedure(csub), pointer :: subind =>null()

   if(ASSOCIATED(subind)) error stop 1_4
   call C_F_PROCPOINTER(C_FUNLOC(csub),subind)
   if(.not. ASSOCIATED(subind)) error stop 2_4

   i = 34_C_INT

   call subind(i, C_FUNLOC(cfun))

   if(i .ne. 22) error stop 3_4

end program procptrBindcFunptr06
