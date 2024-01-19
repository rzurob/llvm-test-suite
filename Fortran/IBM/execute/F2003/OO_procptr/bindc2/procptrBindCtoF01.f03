!*  ===================================================================
!*
!*  DATE                       : 3/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  interlanguage call from C to Fortran.
!*                                Derived type argument passed to Fortran
!*                                and its value gets initialized in Fortran
!*                                and updated in C through procedure pointer
!*                                associating with c function pointer pointing
!*                                to C function.
!* ===================================================================

module mbind1
   use ISO_C_BINDING

   type, bind(c) :: dbind
      integer(C_INT) :: b(5)
   end type

   type, bind(c):: dt
       type(C_FUNPTR) :: cptr
   end type

end module mbind1

integer(C_INT) function fnt1(dobj) bind(c)
   use ISO_C_BINDING
   use mbind1

   interface
       subroutine initdt(i) bind(c)
          use mbind1
          type(dbind), intent(inout) :: i
       end subroutine
   end interface

   type(dbind), intent(inout), target :: dobj

   type(dt) :: dtype

   procedure(initdt), pointer :: fptr => null()
   dtype%cptr = C_NULL_FUNPTR

   if(C_ASSOCIATED(dtype%cptr)) error stop 1_4
   dtype%cptr = C_FUNLOC(initdt)
   if(.not. C_ASSOCIATED(dtype%cptr)) error stop 2_4
   if(.not. C_ASSOCIATED(dtype%cptr, C_FUNLOC(initdt))) error stop 3_4

   if(ASSOCIATED(fptr)) error stop 4_4
   call C_F_PROCPOINTER(dtype%cptr, fptr)
   if(.not. ASSOCIATED(fptr)) error stop 5_4

   dobj%b = 2_C_INT

   call fptr(dobj)

   fnt1 = 0

end function fnt1
