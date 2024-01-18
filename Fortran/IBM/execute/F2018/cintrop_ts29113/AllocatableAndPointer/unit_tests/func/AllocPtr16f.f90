!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Check the following:
!*
!*  The value of the base_addr field of a C descriptor that represents
!*  an unallocated allocatable variable or a pointer that is
!*  disassociated, must be null pointer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test_ptr(p, n) bind(c)
          import
          integer(c_int), pointer :: p
          integer(c_int), value :: n
        end
        subroutine test_alloc(a, n) bind(c)
          import
          integer(c_int), allocatable :: a
          integer(c_int), value :: n
        end
      end interface

      integer(c_int), allocatable :: al
      integer(c_int), pointer :: ptr
      integer(c_int), target :: t = 6

      ptr => NULL()

      call test_ptr(ptr, 1)
      call test_alloc(al, 2)

      allocate(al)
      al = 5
      ptr => t

      deallocate(al)
      ptr => NULL()

      call test_ptr(ptr, 3)
      call test_alloc(al, 4)

      allocate(ptr)
      ptr = 6
      deallocate(ptr)

      call test_ptr(ptr, 5)
      call test_alloc(al, 6)


      end


