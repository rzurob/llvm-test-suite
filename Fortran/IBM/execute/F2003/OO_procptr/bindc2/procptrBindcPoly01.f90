!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 3/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                :  allocate unlimited poly with expression
!*                                in SOURCE being function reference using 
!*                                procedure pointer pointing to C function.
!*                                Check function return value, updated
!*                                argument value as well as selector with
!*                                c interoperable type inside select type.
!*                                dummy argument is C_PTR and function
!*                                return is C_INT. 
!* ===================================================================

program procptrBindcPoly01 

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNPTR, C_INT, C_FUNLOC, C_ASSOCIATED, C_LOC, C_PTR

   type, bind(c):: dt
       type(C_FUNPTR) :: cfunptr 
   end type

   interface
       integer(C_INT) function cfunc(i) bind(c)
          import C_INT, C_PTR
          type(C_PTR) :: i
       end function
   end interface

   type(dt) :: dtype
   integer(C_INT), target :: i
   type(C_PTR) :: j
   integer(C_INT), pointer :: p

   procedure(cfunc), pointer :: funptr =>null()

   class(*), allocatable :: fpoly

   i = max(23_C_INT, 34_C_INT)
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

   i = 34_C_INT
   j = C_LOC(i)

   p=> i

   allocate(fpoly, source=funptr(j))

   if ( .not. C_ASSOCIATED(j) ) error stop 25_4
   if ( C_ASSOCIATED(j, C_LOC(i)) ) error stop 26_4

   if (p /= 34 ) error stop 27_4
   call C_F_POINTER(j,p)
   if ( ASSOCIATED(p,i) ) error stop 28_4
   if ( p /= 22 ) error stop 29_4

   select type (fpoly)
       type is (integer(C_INT))
           if(fpoly .ne. 22_C_INT) error stop 30_4 
       class default
           error stop 31_4 
   end select

   deallocate(fpoly)

end program procptrBindcPoly01 

