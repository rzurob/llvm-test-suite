! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/bindc2/procptrBindcProc32.f
! opt variations: -qnok -ql

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
!*                                nested derived type.
!* ===================================================================

program procptrBindcProc32

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNPTR, C_INT, C_FUNLOC, C_ASSOCIATED, C_LOC, C_PTR

   type, bind(c) :: dt0
      integer(C_INT) :: a
   end type

   type, bind(c) :: dt1
      integer(C_INT) :: b
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      integer(C_INT) :: c
      type(dt1) :: d1
   end type

   type dt(k1)    ! (4)
       integer, kind :: k1
       type(C_FUNPTR) :: cfunptr
   end type

   interface
       type(C_PTR) function cfunc(i) bind(c)
          import C_PTR
          type(C_PTR), VALUE :: i
       end function
   end interface

   type(dt(4)) :: dtype

   type(dt2) , target :: i
   type(C_PTR) :: j, res
   type(dt2), pointer :: p

   procedure(cfunc), pointer :: funptr => null()

   i%c = 1_C_INT
   i%d1%b = 2_C_INT
   i%d1%d0%a = 3_C_INT

   j = C_LOC(i)
   if ( .not. C_ASSOCIATED(j) ) error stop 1_4
   if ( .not. C_ASSOCIATED(j, C_LOC(i)) ) error stop 2_4

   dtype%cfunptr = C_FUNLOC(cfunc)
   if(.not. C_ASSOCIATED(dtype%cfunptr)) error stop 3_4
   if(.not. C_ASSOCIATED(dtype%cfunptr, C_FUNLOC(cfunc))) error stop 4_4

   ! derived type component as CPTR in C_F_PROCPOINTER
   if(ASSOCIATED(funptr)) error stop 5_4
   call C_F_PROCPOINTER(dtype%cfunptr, funptr)
   if(.not. ASSOCIATED(funptr)) error stop 6_4

   p=>i

   res = funptr(j)
   if (.not. C_ASSOCIATED(j) ) error stop 7_4

   call C_F_POINTER(res,p)
   if ( ASSOCIATED(p,i) ) error stop 8_4

   if(p%c /=11) error stop 9_4
   if(p%d1%b /= 22) error stop 10_4
   if(p%d1%d0%a /= 33) error stop 11_4

end program procptrBindcProc32

