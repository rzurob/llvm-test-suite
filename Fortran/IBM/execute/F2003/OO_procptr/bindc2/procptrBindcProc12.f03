!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                                associate procedure pointer with c function
!*                                pointer pointing to C function with void pointer
!*                                as its argument(in Fortran, dummy argument for C_PTR
!*                                is with value attribute) and its return. void * with
!*                                c_least32_t.
!* ===================================================================

program procptrBindcProc12

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNPTR, C_INT_LEAST32_T, C_FUNLOC, C_ASSOCIATED, C_LOC, C_PTR

   type:: dt
       type(C_FUNPTR) :: cfunptr
   end type

   interface
       type(C_PTR) function cfunc(i) bind(c)
          import C_PTR
          type(C_PTR),VALUE :: i
       end function
   end interface

   type(dt) :: dtype
   integer(C_INT_LEAST32_T), target :: i
   type(C_PTR) :: j , res
   integer(C_INT_LEAST32_T), pointer :: p, pp

   procedure(cfunc), pointer :: funptr=>null()

   i = 34_C_INT_LEAST32_T
   j = C_LOC(i)

   if ( .not. C_ASSOCIATED(j) ) error stop 1_4
   if ( .not. C_ASSOCIATED(j, C_LOC(i)) ) error stop 2_4

   dtype%cfunptr = C_FUNLOC(cfunc)
   if(.not. C_ASSOCIATED(dtype%cfunptr)) error stop 21_4
   if(.not. C_ASSOCIATED(dtype%cfunptr, C_FUNLOC(cfunc))) error stop 22_4

   ! derived type component as CPTR in C_F_PROCPOINTER
   if(ASSOCIATED(funptr)) error stop 23_4
   call C_F_PROCPOINTER(dtype%cfunptr, funptr)
   if(.not. ASSOCIATED(funptr)) error stop 24_4

   p=> i
   pp=>i

   res = funptr(j)
   if ( .not. C_ASSOCIATED(j) ) error stop 25_4

   if (p /= 34 ) error stop 26_4
   call C_F_POINTER(j,p)
   if ( p /= 34 ) error stop 27_4

   if (pp /= 34 ) error stop 28_4
   call C_F_POINTER(res,pp)
   if ( ASSOCIATED(pp,i) ) error stop 29_4
   if ( pp /= 22 ) error stop 30_4

end program procptrBindcProc12