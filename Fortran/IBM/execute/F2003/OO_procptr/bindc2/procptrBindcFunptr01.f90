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
!*                              pointer reference. Argument value is updated
!*                              by calling C function.
!* ===================================================================

module fptr01
   use ISO_C_BINDING
   interface
       subroutine csub(i, cptr) bind(c)
          import C_INT, C_FUNPTR
          integer(C_INT) :: i
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr01

program procptrBindcFunptr01

   use ISO_C_BINDING

   use fptr01

   interface
       integer(C_INT) function cfun(i) bind(c)
          import C_INT
          integer(C_INT),value :: i
       end function
   end interface

   integer(C_INT) :: i

   procedure(csub), pointer :: subind

   subind => csub

   i = 34_C_INT

   call subind(i, C_FUNLOC(cfun))

   if(i .ne. 22) error stop 1_4

end program procptrBindcFunptr01
