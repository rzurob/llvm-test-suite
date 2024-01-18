! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/bindc2/procptrBindCtoF03.f
! opt variations: -qnok -qnol

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang
!*  DATE                       : 3/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DESCRIPTION                :  interlanguage call from C->F/F->C
!*                                Derived type argument passed to Fortran
!*                                and its value initialized by Fortran
!*                                by associating C function address with 
!*                                procedure pointer.
!*                                Derived type argument passed to Fortran
!*                                again and its value updated by Fortran
!*                                through function pointer call in C.
!* ===================================================================

module mbind3
   use ISO_C_BINDING, only : C_INT, C_FUNPTR

   type, bind(c) :: dbind
      integer(C_INT) :: b(5)
   end type

   type dt(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
       type(C_FUNPTR) :: cptr
   end type

end module mbind3

integer(C_INT) function fnt1(dobj) bind(c)
   use ISO_C_BINDING, only : C_INT, C_PTR, C_NULL_FUNPTR
   use mbind3

   interface
       subroutine initdt(i) bind(c)
          use ISO_C_BINDING, ONLY : C_PTR
          type(C_PTR) :: i
       end subroutine 
   end interface

   type(dbind), intent(inout), target :: dobj

   type(dt(4,20)) :: dtype

   type(C_PTR) :: a

   type(dbind), pointer :: aa

   procedure(initdt), pointer :: fptr => null()

   dobj%b = 2_C_INT
   dtype%cptr = C_NULL_FUNPTR

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

integer(C_INT) function fnt2(dobj) bind(c)
   use ISO_C_BINDING, only : C_INT, C_PTR, C_FUNPTR, C_NULL_FUNPTR
   use mbind3

   interface
       subroutine update(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine update 
   end interface

   interface
       subroutine updatedt(i) bind(c)
          import dbind
          type(dbind) :: i
       end subroutine updatedt
   end interface

   procedure(update), pointer :: pupdate => null()
   procedure(updatedt), pointer :: pupdatedt => null()

   type(dbind), intent(inout), target :: dobj

   type(C_FUNPTR) ::fp

   fp = C_NULL_FUNPTR

   pupdate => update

   call pupdate(fp)

   if(ASSOCIATED(pupdatedt)) error stop 30_4    
   call C_F_PROCPOINTER(fp, pupdatedt)
   if(.not. ASSOCIATED(pupdatedt)) error stop 31_4

   call pupdatedt(dobj)

   fnt2 = 0
 
end function fnt2
