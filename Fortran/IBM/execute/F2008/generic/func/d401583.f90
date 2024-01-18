module f2008_generic
implicit none

   type Base
      integer :: i
   end type

   interface assignment(=)
      module procedure assgn_alloc
      module procedure assgn_ptr
   end interface

   contains
      subroutine assgn_alloc (x, y)
      implicit none
         type(Base), intent(inout), allocatable :: x
         type(Base), intent(in) :: y

         x%i = y%i + 10
         print *, "   inside assgn_alloc()  x%i=",x%i, "  y%i=",y%i
      end subroutine

      subroutine assgn_ptr (x, y)
      implicit none
         type(Base), intent(inout), pointer :: x
         type(Base), intent(in) :: y

         x%i = y%i - 100
         print *, "   inside assgn_ptr()  x%i=",x%i, "  y%i=",y%i
      end subroutine
end module

program generic_allocatable_pointer_f030
use f2008_generic
implicit none
   type(Base), allocatable, target :: Base_alloc_tgt
   type(Base), allocatable         :: Base_alloc
   type(Base), pointer             :: Base_ptr
   type(Base)                      :: base_val, Base_res

   allocate(Base_alloc_tgt, source = Base(700) )
   allocate(Base_alloc, source = Base(4000) )
   Base_ptr => Base_alloc_tgt

   base_val%i = 999
   Base_res = base_val
print *,Base_res
   Base_alloc = Base_res
print *,Base_alloc
   Base_ptr = Base_res
print *,Base_ptr

end program
