! *********************************************************************
!* ===================================================================
!*
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - Allocatable scalar/array of various interoperable types
!*                                - reshape()
!*                                - Allocatable summy arg. as actual arg. associated with
!*                                   - allocatable array
!*                                   - assumed shape array
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program AllocatableDummyArgument112f
  implicit none
  integer i, n, k
  real s
  real, allocatable :: a3(:,:,:,:)

  interface
    subroutine use_reshape(arg, ul) bind(c)
      use iso_c_binding
      implicit none
      integer i, ul
      real(c_float), allocatable :: arg(:,:,:,:)
    end subroutine use_reshape
    subroutine reverse(arg, n) bind(c)
      use iso_c_binding
      implicit none
      integer i, k, n
      real(c_float), allocatable :: arg(:,:,:,:)
    end subroutine reverse
  end interface


   allocate(a3(0:8,0:8,0:8,0:8))
   n = 7

   call use_reshape(a3,n)

   call reverse(a3,n)

   s = sum(a3)
   write (6,'(F24.7)') s

end program AllocatableDummyArgument112f

subroutine use_reshape(arg, ul) bind(c)
  use iso_c_binding
  implicit none
  integer i, ul
  real(c_float), allocatable :: arg(:,:,:,:)

  interface
    subroutine check_alloc(a)
       implicit none
       real, allocatable :: a(:,:,:,:)
    end
    subroutine check_assumed(a)
       implicit none
       real a(:,:,:,:)
    end
    subroutine check_dim(a)
       implicit none
       real, dimension(:,:,:,:) :: a
    end
    subroutine check_alloc_c(a) bind(c)
       use iso_c_binding
       implicit none
       real(c_float), allocatable :: a(:,:,:,:)
    end
  end interface

  arg(:,:,:,:)=reshape( (/(1.*i,i=1,(ul+2)*(ul+2)*(ul+2)*(ul+2))/), (/ul+2,ul+2,ul+2,ul+2/) )

  call check_alloc(arg)
  call check_assumed(arg)
  call check_dim(arg)
end subroutine use_reshape

subroutine reverse(arg, n) bind(c)
  use iso_c_binding
  implicit none
  integer i, k, n
  real(c_float), allocatable :: arg(:,:,:,:)

  do i = 1,2
    do k = 1,2
      arg(1:n,k:k+2,1:n,1:n) = arg(0:n-1,k+1:k+3,0:n-1,1:n) + arg(2:n+1,1:3,0:n-1,1:n)
    enddo
  enddo
end subroutine reverse

subroutine check_alloc(a)
  implicit none
  real, allocatable :: a(:,:,:,:)

  if (lbound(a,1) .ne. 0) error stop 10
  if (ubound(a,1) .ne. 8) error stop 11

  if (lbound(a,2) .ne. 0) error stop 12
  if (ubound(a,2) .ne. 8) error stop 13

  if (lbound(a,3) .ne. 0) error stop 14
  if (ubound(a,3) .ne. 8) error stop 15

  if (lbound(a,4) .ne. 0) error stop 16
  if (ubound(a,4) .ne. 8) error stop 17
end

subroutine check_assumed(a)
  implicit none
  real a(:,:,:,:)

  if (lbound(a,1) .ne. 1) error stop 20
  if (ubound(a,1) .ne. 9) error stop 21

  if (lbound(a,2) .ne. 1) error stop 22
  if (ubound(a,2) .ne. 9) error stop 23

  if (lbound(a,3) .ne. 1) error stop 24
  if (ubound(a,3) .ne. 9) error stop 25

  if (lbound(a,4) .ne. 1) error stop 26
  if (ubound(a,4) .ne. 9) error stop 27

end
subroutine check_dim(a)
  implicit none
  real, dimension(:,:,:,:) :: a

  if (lbound(a,1) .ne. 1) error stop 30
  if (ubound(a,1) .ne. 9) error stop 31

  if (lbound(a,2) .ne. 1) error stop 32
  if (ubound(a,2) .ne. 9) error stop 33

  if (lbound(a,3) .ne. 1) error stop 34
  if (ubound(a,3) .ne. 9) error stop 35

  if (lbound(a,4) .ne. 1) error stop 36
  if (ubound(a,4) .ne. 9) error stop 37
end

subroutine check_alloc_c(a) bind(c)
  use iso_c_binding
  implicit none
  real(c_float), allocatable :: a(:,:,:,:)

  if (lbound(a,1) .ne. 0) error stop 40
  if (ubound(a,1) .ne. 8) error stop 41

  if (lbound(a,2) .ne. 0) error stop 42
  if (ubound(a,2) .ne. 8) error stop 43

  if (lbound(a,3) .ne. 0) error stop 44
  if (ubound(a,3) .ne. 8) error stop 45

  if (lbound(a,4) .ne. 0) error stop 46
  if (ubound(a,4) .ne. 8) error stop 47
end
