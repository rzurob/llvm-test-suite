!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                              pass null function pointer p to C function
!*                              pointer, get address of C function. In
!*                              Fortran, associating p to procedure pointer.
!*                              Referencing procedure pointer entity and
!*                              check the correctness of the return value.
!* ===================================================================

module fptr10
   use ISO_C_BINDING

   type,bind(c) :: dt
       type(C_FUNPTR) :: fp
       integer(C_INT) :: ig
   end type

   interface
       subroutine csub(cptr) bind(c)
          import dt
          type(dt) :: cptr
       end subroutine csub
   end interface
end module fptr10

program procptrBindcFunptr10

   use ISO_C_BINDING

   use fptr10

   interface
       integer(C_INT) function cfun(i) bind(c)
          import C_INT
          integer(C_INT), value :: i
       end function
   end interface

   type(dt) :: dt_obj
   integer(C_INT) :: i, j

   procedure(csub), pointer :: subind=>null()
   procedure(cfun), pointer :: funind=>null()

   dt_obj%ig = 34_C_INT
   dt_obj%fp = C_NULL_FUNPTR

   i = 11_C_INT
   j = 22_C_INT

   subind => csub

   if(C_ASSOCIATED(C_FUNLOC(cfun), dt_obj%fp)) error stop 1_4
   call subind(dt_obj)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun), dt_obj%fp)) error stop 2_4

   if(ASSOCIATED(funind)) error stop 3_4
   call C_F_PROCPOINTER(dt_obj%fp, funind)
   if(.not. ASSOCIATED(funind)) error stop 4_4

   if( i .ne. 11_C_INT) error stop 5_4

   i = funind(j)

   if(i .ne. 22_C_INT) error stop 6_4

end program procptrBindcFunptr10
