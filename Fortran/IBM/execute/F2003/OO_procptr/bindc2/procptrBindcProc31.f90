!*  ===================================================================
!*
!*  DATE                       : 3/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                              associate procedure pointer with C function with
!*                              void * as argument. Actual argument in Fortran is
!*                              one-dimensional /multidimensional array.
!* ===================================================================

program procptrBindcProc31

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_NULL_FUNPTR, C_FUNPTR, C_INT, C_FUNLOC, C_ASSOCIATED, C_PTR

   type dt
       type(C_FUNPTR) :: cptr
       type(C_FUNPTR) :: cpptr
   end type

   interface
       subroutine csubarray(i) bind(c)
          use ISO_C_BINDING, ONLY : C_PTR
          type(C_PTR),VALUE :: i
       end subroutine csubarray
       subroutine csubarray2(i) bind(c)
          use ISO_C_BINDING, ONLY : C_PTR
          type(C_PTR),VALUE :: i
       end subroutine csubarray2
   end interface

   type(dt) :: dtype

   integer(C_INT), parameter :: da = 4, db= 5
   integer(C_INT) i
   integer(C_INT), allocatable, target  :: x(:), y(:, :)
   integer(C_INT), pointer :: pxy(:), pxy2(:,:)
   type(C_PTR) :: pc, pcc

   procedure(csubarray), pointer :: fptr => null()
   procedure(csubarray2), pointer :: fpptr => null()

   dtype%cptr=C_NULL_FUNPTR
   dtype%cpptr=C_NULL_FUNPTR

   ! test 1: one dimensional array

   allocate(x(da))

   do i =1, da
      x(i) = i
   end do

   pc = C_LOC(x)
   if ( .not. C_ASSOCIATED(pc) ) error stop 1_4
   if ( .not. C_ASSOCIATED(pc, C_LOC(x)) ) error stop 2_4

   pxy=>x

   if(C_ASSOCIATED(dtype%cptr)) error stop 3_4

   dtype%cptr = C_FUNLOC(csubarray)
   if(.not. C_ASSOCIATED(dtype%cptr)) error stop 4_4
   if(.not. C_ASSOCIATED(dtype%cptr, C_FUNLOC(csubarray))) error stop 5_4

   ! derived type component as CPTR in C_F_PROCPOINTER
   if(ASSOCIATED(fptr)) error stop 6_4
   call C_F_PROCPOINTER(dtype%cptr, fptr)
   if(.not. ASSOCIATED(fptr)) error stop 7_4

   call fptr(pc)
   if (.not. C_ASSOCIATED(pc) ) error stop 8_4

   do i = 1, da
      if ( pxy(i) /= i+2 ) error stop 11_4
   end do

   deallocate(x)

   ! test 2: multi-dimentional array

   allocate(y(db,db))

   do i = 1, db
      do j = 1, db
         y(i,j) = i+j
      end do
   end do

   pcc = C_LOC(y)
   if ( .not. C_ASSOCIATED(pcc) ) error stop 21_4
   if ( .not. C_ASSOCIATED(pcc, C_LOC(y)) ) error stop 22_4

   pxy2=>y

   if(C_ASSOCIATED(dtype%cpptr)) error stop 23_4
   dtype%cpptr = C_FUNLOC(csubarray2)
   if(.not. C_ASSOCIATED(dtype%cpptr)) error stop 24_4
   if(.not. C_ASSOCIATED(dtype%cpptr, C_FUNLOC(csubarray2))) error stop 13_4

   ! derived type component as CPTR in C_F_PROCPOINTER
   if(ASSOCIATED(fpptr)) error stop 25_4
   call C_F_PROCPOINTER(dtype%cpptr, fpptr)
   if(.not. ASSOCIATED(fpptr)) error stop 26_4

   call fpptr(pcc)
   if (.not. C_ASSOCIATED(pcc) ) error stop 27_4

   do i = 1, db
      do j = 1, db
         if ( pxy2(i,j) /= i+j+1 ) error stop 30_4
      end do
   end do

   deallocate(y)

end program procptrBindcProc31

