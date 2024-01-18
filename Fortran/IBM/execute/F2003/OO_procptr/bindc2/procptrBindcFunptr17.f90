!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  function pointer used as expression statement.
!*                                array and C_FUNLOC directly passed to C.
!* ===================================================================

program procptrBindcFunptr17

   use ISO_C_BINDING

   interface
      subroutine bubble(work, size, fptr) bind(c)
         import C_FUNPTR, C_INT
         integer(C_INT) :: size
         integer(C_INT) :: work(10)
         type(C_FUNPTR) :: fptr
      end subroutine
   end interface

   interface
       integer(C_INT) function ascending(i, j) bind(c)
          import C_INT
          integer(C_INT) :: i, j
          value :: i,j
       end function
   end interface

   type(C_FUNPTR) :: p
   integer(C_INT) :: a(10) = (/2,6, 4, 8, 10, 12, 89, 68, 45, 37/)
   integer(C_INT) :: size

   procedure(bubble), pointer :: csub => null()

   p = C_NULL_FUNPTR
   size = 10_C_INT

   if(ASSOCIATED(csub)) error stop 1_4
   call C_F_PROCPOINTER(C_FUNLOC(bubble), csub)
   if(.not. ASSOCIATED(csub)) error stop 2_4

   if(a(1) .ne. 2 .or. a(2) .ne. 6 .or. a(3) .ne. 4 .or. a(4) .ne. 8 .or. a(5) .ne. 10 .or. a(6) .ne. 12 .or. a(7) .ne. 89 .or. a(8) .ne. 68 .or. a(9) .ne. 45 .or. a(10) .ne. 37 ) error stop 3_4

   call csub(a, size, C_FUNLOC(ascending))

   if(a(1) .ne. 2 .or. a(2) .ne. 4 .or. a(3) .ne. 6 .or. a(4) .ne. 8 .or. a(5) .ne. 10 .or.a(6) .ne. 12 .or. a(7) .ne. 37 .or. a(8) .ne. 45 .or. a(9) .ne. 68 .or. a(10) .ne. 89 ) error stop 4_4

end program procptrBindcFunptr17
