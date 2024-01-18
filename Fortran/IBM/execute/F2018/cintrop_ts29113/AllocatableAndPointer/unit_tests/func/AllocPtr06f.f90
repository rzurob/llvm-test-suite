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
!*  DESCRIPTION                : Doing Fortran I/O when CFIs are involved
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      logical precision_r4
      interface
        subroutine getInput(arg1, arg2, arg3, arg4) bind(c)
          import
          real(c_float), pointer :: arg1(:,:)
          integer(c_int), pointer :: arg2
          integer(c_int), allocatable :: arg3(:,:)
          real(c_float), allocatable :: arg4
        end
      end interface

      real(c_float), pointer :: p1(:,:)
      integer(c_int), pointer :: p2
      integer(c_int), allocatable :: al1(:,:)
      real(c_float), allocatable :: al2

      real(c_float) :: verify1(3,2)
      integer(c_int) :: verify2(3,2)

      integer :: i, j
      logical :: ll

      verify1(1,1) = 1.0; verify1(1,2) = 2.0;
      verify1(2,1) = 3.0; verify1(2,2) = 4.0;
      verify1(3,1) = 5.0; verify1(3,2) = 6.0;

      verify2 = -verify1

      allocate(p1(3,2))
      allocate(p2)
      allocate(al1(3,2))
      allocate(al2)

      call getInput(p1, p2, al1, al2)

      do i = 1, 2
         do j = 1, 3
            if (.not. precision_r4(p1(j, i), verify1(j, i))) then
               print *, "Unexpected value at p1(", j, ",", i,")"
               error stop 1
            end if
         end do
      end do
      if (any(al1 .ne. verify2)) error stop 2
      if (p2 .ne. 4321) error stop 3
      if (.not. precision_r4(al2, -4321.0)) error stop 4

      end

      subroutine getInput(arg1, arg2, arg3, arg4) bind(c)
        use, intrinsic :: iso_c_binding
        real(c_float), pointer :: arg1(:,:)
        integer(c_int), pointer :: arg2
        integer(c_int), allocatable :: arg3(:,:)
        real(c_float), allocatable :: arg4

        open(10, file="AllocPtr06f.dat")
        read(10, *) arg1
        read(10, *) arg2
        read(10, *) arg3
        read(10, *) arg4
      end
