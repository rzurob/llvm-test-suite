!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                                associate procedure pointer with c function
!*                                pointer pointing to function returning
!*                                C int. Also associate procedure pointer
!*                                with c function pointer pointing to
!*                                subroutine. Check function result as
!*                                well as argument result.
!*                                c function pointer is derived type
!*                                component.
!*                                procedure pointer is struct component.
!* ===================================================================

program procptrBindcProc01b

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNPTR, C_INT, C_FUNLOC, C_ASSOCIATED, C_LOC, C_PTR

   type, bind(c):: dt
       type(C_FUNPTR) :: cptr
       type(C_FUNPTR) :: cfunptr
   end type

   interface
       subroutine csub(i) bind(c)
          import C_PTR
          type(C_PTR) :: i
       end subroutine csub
   end interface
   interface
       type(C_PTR) function cfunc(i) bind(c)
          import C_PTR, C_INT
          type(C_PTR) :: i
       end function
   end interface

   type(dt) :: dtype
   integer(C_INT), target :: i
   type(C_PTR) :: j, res
   integer(C_INT), pointer :: p, pp

   type base
      procedure(csub), nopass, pointer :: fptr
   end type

   type, extends(base) :: child
      procedure(cfunc), nopass, pointer :: funptr
   end type

   type(child) :: c = child(null(), null())

   i = max(23_C_INT, 34_C_INT)
   j = C_LOC(i)
   if ( .not. C_ASSOCIATED(j) ) error stop 1_4
   if ( .not. C_ASSOCIATED(j, C_LOC(i)) ) error stop 2_4

   ! test1 subroutine

   dtype%cptr = C_FUNLOC(csub)
   if(.not. C_ASSOCIATED(dtype%cptr)) error stop 11_4
   if(.not. C_ASSOCIATED(dtype%cptr, C_FUNLOC(csub))) error stop 12_4

   ! derived type component as CPTR in C_F_PROCPOINTER
   if(ASSOCIATED(c%fptr)) error stop 13_4
   call C_F_PROCPOINTER(dtype%cptr, c%fptr)
   if(.not. ASSOCIATED(c%fptr)) error stop 14_4

   i = 34_C_INT
   j = C_LOC(i)

   p => i

   call c%fptr(j)
   if ( .not. C_ASSOCIATED(j) ) error stop 15_4
   if ( C_ASSOCIATED(j, C_LOC(i)) ) error stop 16_4

   if (p /= 34 ) error stop 17_4
   call C_F_POINTER(j,p)
   if ( ASSOCIATED(p,i) ) error stop 18_4
   if ( p /= 23 ) error stop 19_4

   ! test 2 function

   dtype%cfunptr = C_FUNLOC(cfunc)
   if(.not. C_ASSOCIATED(dtype%cfunptr)) error stop 21_4
   if(.not. C_ASSOCIATED(dtype%cfunptr, C_FUNLOC(cfunc))) error stop 22_4

   ! derived type component as CPTR in C_F_PROCPOINTER
   if(ASSOCIATED(c%funptr)) error stop 23_4
   call C_F_PROCPOINTER(dtype%cfunptr, c%funptr)
   if(.not. ASSOCIATED(c%funptr)) error stop 24_4

   i = 34_C_INT
   j = C_LOC(i)

   p=> i
   pp=>i

   res = c%funptr(j)

   if ( .not. C_ASSOCIATED(j) ) error stop 25_4
   if ( C_ASSOCIATED(j, C_LOC(i)) ) error stop 26_4

   if (p /= 34 ) error stop 27_4
   call C_F_POINTER(j,p)
   if ( ASSOCIATED(p,i) ) error stop 28_4
   if ( p /= 22 ) error stop 29_4

   if (pp /= 34 ) error stop 30_4
   call C_F_POINTER(res,pp)
   if ( ASSOCIATED(pp,i) ) error stop 31_4
   if ( pp /= 22 ) error stop 32_4

end program procptrBindcProc01b

