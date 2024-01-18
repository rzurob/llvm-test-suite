!*  ===================================================================
!*
!*  DATE                       : 3/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  interlanguage call from C->F/F->C
!*                                Derived type argument passed to Fortran
!*                                and its value initialized by Fortran
!*                                by associating C function address with
!*                                procedure pointer.
!*                                Derived type argument passed to Fortran
!*                                again and its value updated by Fortran
!*                                through function pointer call in C.
!*                                Derived type is nested structure.
!* ===================================================================

module mbind4
   use ISO_C_BINDING, only : C_INT, C_FUNPTR

   type, bind(c) :: dbind0
      integer(C_INT) :: b0(5)
   end type

   type, bind(c) :: dbind1
      integer(C_INT) :: b1(5)
      type(dbind0) :: d0
   end type

   type, bind(c) :: dbind2
      integer(C_INT) :: b2(5)
      type(dbind1) :: d1
   end type

end module mbind4

integer(C_INT) function fnt0(dobj) bind(c)
   use ISO_C_BINDING, only : C_INT, C_PTR
   use mbind4

   interface
       subroutine initdt0(i) bind(c)
          use ISO_C_BINDING, ONLY : C_PTR
          type(C_PTR), VALUE :: i
       end subroutine
   end interface

   type(dbind0), intent(inout), target :: dobj

   type(C_PTR) :: a

   type(dbind0), pointer :: aa

   procedure(initdt0), pointer :: fptr => null()

   dobj%b0 = 2_C_INT

   a = C_LOC(dobj)
   if ( .not. C_ASSOCIATED(a) ) error stop 1_4
   if ( .not. C_ASSOCIATED(a, C_LOC(dobj)) ) error stop 2_4

   aa=>dobj

   if(ASSOCIATED(fptr)) error stop 6_4
   call C_F_PROCPOINTER(C_FUNLOC(initdt0), fptr)
   if(.not. ASSOCIATED(fptr)) error stop 7_4

   call fptr(a)
   if (.not. C_ASSOCIATED(a) ) error stop 8_4

   call C_F_POINTER(a, aa)

   do i = 1, 5
      if ( aa%b0(i) /= 2 ) error stop 11
      aa%b0(i) = i+1
   end do

   do i = 1, 5
      dobj%b0(i) = aa%b0(i)
   end do

   fnt0 = 0

end function fnt0

integer(C_INT) function fnt1(dobj) bind(c)
   use ISO_C_BINDING, only : C_INT, C_PTR
   use mbind4

   interface
       subroutine initdt1(i) bind(c)
          use ISO_C_BINDING, ONLY : C_PTR
          type(C_PTR), VALUE :: i
       end subroutine
   end interface

   type(dbind1), intent(inout), target :: dobj

   type(C_PTR) :: a

   type(dbind1), pointer :: aa

   procedure(initdt1), pointer :: fptr => null()

   dobj%b1 = 2_C_INT
   dobj%d0%b0 = 2_C_INT

   a = C_LOC(dobj)
   if ( .not. C_ASSOCIATED(a) ) error stop 21_4
   if ( .not. C_ASSOCIATED(a, C_LOC(dobj)) ) error stop 22_4

   aa=>dobj

   if(ASSOCIATED(fptr)) error stop 26_4
   call C_F_PROCPOINTER(C_FUNLOC(initdt1), fptr)
   if(.not. ASSOCIATED(fptr)) error stop 27_4

   call fptr(a)
   if (.not. C_ASSOCIATED(a) ) error stop 28_4

   call C_F_POINTER(a, aa)

   do i = 1, 5
      if ( aa%b1(i) /= 2 ) error stop 31_4
      aa%b1(i) = i+1
      aa%d0%b0(i) = i+1
   end do

   do i = 1, 5
      dobj%b1(i) = aa%b1(i)
      dobj%d0%b0(i) = aa%d0%b0(i)
   end do

   fnt1 = 0

end function fnt1

integer(C_INT) function fnt2(dobj) bind(c)
   use ISO_C_BINDING, only : C_INT, C_PTR
   use mbind4

   interface
       subroutine initdt2(i) bind(c)
          use ISO_C_BINDING, ONLY : C_PTR
          type(C_PTR), VALUE :: i
       end subroutine
   end interface

   type(dbind2), intent(inout), target :: dobj

   type(C_PTR) :: a

   type(dbind2), pointer :: aa

   procedure(initdt2), pointer :: fptr => null()

   dobj%b2 = 2_C_INT
   dobj%d1%b1 = 2_C_INT
   dobj%d1%d0%b0 = 2_C_INT

   a = C_LOC(dobj)
   if ( .not. C_ASSOCIATED(a) ) error stop 41_4
   if ( .not. C_ASSOCIATED(a, C_LOC(dobj)) ) error stop 42_4

   aa=>dobj

   if(ASSOCIATED(fptr)) error stop 46_4
   call C_F_PROCPOINTER(C_FUNLOC(initdt2), fptr)
   if(.not. ASSOCIATED(fptr)) error stop 47_4

   call fptr(a)
   if (.not. C_ASSOCIATED(a) ) error stop 48_4

   call C_F_POINTER(a, aa)

   do i = 1, 5
      if ( aa%b2(i) /= 2 ) error stop 51_4
      aa%b2(i) = i+1
      aa%d1%b1(i) = i+1
      aa%d1%d0%b0(i) = i+1
   end do

   do i = 1, 5
      dobj%b2(i) = aa%b2(i)
      dobj%d1%b1(i) = aa%d1%b1(i)
      dobj%d1%d0%b0(i) = aa%d1%d0%b0(i)
   end do

   fnt2 = 0

end function fnt2

integer(C_INT) function fnt(dobj) bind(c)
   use ISO_C_BINDING, only : C_INT, C_PTR, C_FUNPTR, C_NULL_FUNPTR
   use mbind4

   interface
       subroutine update(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine update
   end interface

   interface
       subroutine updatedt(i) bind(c)
          import dbind2
          type(dbind2) :: i
       end subroutine updatedt
   end interface

   procedure(update), pointer :: pupdate => null()
   procedure(updatedt), pointer :: pupdatedt => null()

   type(dbind2), intent(inout), target :: dobj

   type(C_FUNPTR) ::fp

   fp = C_NULL_FUNPTR

   pupdate => update

   call pupdate(fp)

   if(ASSOCIATED(pupdatedt)) error stop 60_4
   call C_F_PROCPOINTER(fp, pupdatedt)
   if(.not. ASSOCIATED(pupdatedt)) error stop 61_4

   call pupdatedt(dobj)

   fnt = 0

end function fnt
