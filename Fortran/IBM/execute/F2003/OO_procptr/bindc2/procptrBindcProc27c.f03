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
!*                                void ** as its argument and void * as its return.
!*                                void * with char.
!* ===================================================================

program procptrBindcProc27c

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNPTR, C_CHAR, C_FUNLOC, C_ASSOCIATED, C_LOC, C_PTR

   type dt
       type(C_FUNPTR) :: cfunptr
   end type
   interface
       type(C_PTR) function cfunc(i) bind(c)
          import C_PTR
          type(C_PTR) :: i
       end function
   end interface

   type(dt) :: dtype
   character(C_CHAR), target :: i
   type(C_PTR) :: j, res
   character(C_CHAR), pointer :: p, pp

   procedure(cfunc), pointer :: funptr => null()

   i = 'A'
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

   if (p /= 'A') error stop 26_4
   call C_F_POINTER(j,p)
   if ( p /= 'C' ) error stop 27_4

   if (pp /= 'A') error stop 28_4
   call C_F_POINTER(res,pp)
   if ( ASSOCIATED(pp,i) ) error stop 29_4
   if ( pp /= 'C' ) error stop 30_4

end program procptrBindcProc27c
