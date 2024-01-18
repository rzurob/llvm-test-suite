!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Non-BIND(C) Fortran calling BIND(C)
!*                               procedure defined in Fortran.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine foo(arg1, arg2) bind(c)
           use, intrinsic :: iso_c_binding
           real(c_double), pointer, intent(out) :: arg1(:, :)
           integer(c_int), allocatable, intent(out) :: arg2(:, :)
         end
      end interface
      real(c_double), pointer :: ptr(:,:)
      integer(c_int), allocatable :: all(:,:)

      call foo(ptr, all)

      if (associated(ptr)) then
         print '(f6.2)', ptr
         deallocate(ptr)
         if (associated(ptr)) then
            error stop 1
         end if
      else
         print *, "Error: ptr was not associated"
         error stop 2
      end if
      if (allocated(all)) then
         print *, all
         deallocate(all)
         if (allocated(all)) then
            error stop 3
         end if
      else
         print *, "Error: all was not allocated"
         error stop 4
      end if

      end

      subroutine foo(arg1, arg2) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none
        real(c_double), pointer, intent(out) :: arg1(:,:)
        integer(c_int), allocatable, intent(out) :: arg2(:,:)
        integer, parameter :: rows = 10, columns = 2
        integer :: i, j
        allocate(arg1(rows, columns))
        do i = 1, columns, 1
           do j = 1, rows, 1
              arg1(j, i) = i*j;
           end do
        end do

        allocate(arg2(rows, columns))
        do i = 1, columns, 1
           do j = 1, rows, 1
              arg2(j, i) = -i*j;
           end do
        end do

      end
