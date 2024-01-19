!*  ===================================================================
!*
!*  DATE                       : 3/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  interlanguage call from C to Fortran.
!*                                Derived type argument passed to Fortran
!*                                and its value updated by Fortran.
!*                                procedure pointer declared inside
!*                                fortran subprogram and it is associated with
!*                                c function pointer pointing to C
!*                                function.
!* ===================================================================

module mbind2
   use ISO_C_BINDING, only : C_INT, C_FUNPTR

   type, bind(c) :: dbind
      integer(C_INT) :: b(5)
   end type

   type dt
       type(C_FUNPTR) :: cptr
   end type

end module mbind2

integer(C_INT) function fnt1(dobj) bind(c)
   use ISO_C_BINDING, only : C_INT, C_PTR, C_NULL_FUNPTR
   use mbind2

   interface
       subroutine initdt(i) bind(c)
          use ISO_C_BINDING, ONLY : C_PTR
          type(C_PTR) :: i
       end subroutine
   end interface

   type(dbind), intent(inout), target :: dobj

   type(dt) :: dtype

   type(C_PTR) :: a

   type(dbind), pointer :: aa

   procedure(initdt), pointer :: fptr => null()

   dtype%cptr= C_NULL_FUNPTR

   dobj%b = 2_C_INT

   a = C_LOC(dobj)
   if ( .not. C_ASSOCIATED(a) ) error stop 1_4
   if ( .not. C_ASSOCIATED(a, C_LOC(dobj)) ) error stop 2_4

   aa=>dobj

   if(C_ASSOCIATED(dtype%cptr)) error stop 3_4
   dtype%cptr = C_FUNLOC(initdt)
   if(.not. C_ASSOCIATED(dtype%cptr)) error stop 4_4
   if(.not. C_ASSOCIATED(dtype%cptr, C_FUNLOC(initdt))) error stop 5_4

   if(ASSOCIATED(fptr)) error stop 6_4
   call C_F_PROCPOINTER(dtype%cptr, fptr)
   if(.not. ASSOCIATED(fptr)) error stop 7_4

   call fptr(a)
   if (.not. C_ASSOCIATED(a) ) error stop 8_4
   if (C_ASSOCIATED(a,C_LOC(dobj)) ) error stop 9_4

   call C_F_POINTER(a, aa)
   if(ASSOCIATED(aa, dobj)) error stop 10_4

   do i = 1, 5
      if ( aa%b(i) /= i ) error stop 20
      aa%b(i) = i+1
   end do

   do i = 1, 5
      dobj%b(i) = aa%b(i)
   end do

   fnt1 = 0

end function fnt1
