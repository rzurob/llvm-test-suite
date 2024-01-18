! *********************************************************************
!* ===================================================================
!*
!* DATE                         : January 25, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - type c_int
!*                                - Call to BIND(C) procedure from different scopes:
!*                                main program, module and internal procedure
!*                                - LBOUND, UBOUND, SHAPE, SIZE
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
   implicit none
   integer, parameter   :: dim1 = 16

   interface
     subroutine sub_bind_c(dim1, A, B, C, D, t_size, t_shape, t_lbound, t_ubound) bind(C)
       use iso_c_binding
       implicit none
       integer :: dim1
       integer :: t_size(50), t_shape(50,10), t_lbound(50,10), t_ubound(50,10)
       integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)
     end subroutine sub_bind_c
   end interface

   contains
   subroutine sub_mod(t_size, t_shape, t_lbound, t_ubound)
     use iso_c_binding
     integer :: i, j, k
     integer :: t_size(50), t_shape(50,10), t_lbound(50,10), t_ubound(50,10)
     integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

     allocate (A(dim1, dim1))
     if (.not. allocated(A))  stop 110
     allocate (B(dim1, dim1))
     if (.not. allocated(B))  stop 111
     allocate (C(dim1, dim1))
     if (.not. allocated(C))  stop 112
     allocate (D(1,2,3,4,5,1,2,3,4,5))
     if (.not. allocated(D))  stop 113

     do i = 1, dim1
       do j = 1, dim1
         A(i,j) = i + j
         B(i,j) = i - j
       end do
     end do

     C = 0; D = 0;

     do i = 1, dim1
        do j = 1, dim1
           do k = 1, dim1
              C = C + A(i,k)*B(k,j)
           end do
           D = D + A(i,j)
        end do
     end do

     t_size(41)        = size(A)
     t_shape(41,1:2)   = shape(A)
     t_lbound(41,1:2)  = lbound(A)
     t_ubound(41,1:2)  = ubound(A)
     t_size(42)        = size(B)
     t_shape(42,1:2)   = shape(B)
     t_lbound(42,1:2)  = lbound(B)
     t_ubound(42,1:2)  = ubound(B)
     t_size(43)        = size(C)
     t_shape(43,1:2)   = shape(C)
     t_lbound(43,1:2)  = lbound(C)
     t_ubound(43,1:2)  = ubound(C)
     t_size(44)        = size(D)
     t_shape(44,1:10)  = shape(D)
     t_lbound(44,1:10) = lbound(D)
     t_ubound(44,1:10) = ubound(D)

!---------- call BIND(C) procedure from Module procedure
   call sub_bind_c(dim1, A, B, C, D, t_size, t_shape, t_lbound, t_ubound)

   if ( t_size(41)          /=  t_size(31))       stop 114
   if ( any(t_shape(41,:)   /=  t_shape(31,:)))   stop 115
   if ( any(t_lbound(41,:)  /=  t_lbound(31,:)))  stop 116
   if ( any(t_ubound(41,:)  /=  t_ubound(31,:)))  stop 117
   if ( t_size(42)          /=  t_size(32))       stop 118
   if ( any(t_shape(42,:)   /=  t_shape(32,:)))   stop 119
   if ( any(t_lbound(42,:)  /=  t_lbound(32,:)))  stop 120
   if ( any(t_ubound(42,:)  /=  t_ubound(32,:)))  stop 121
   if ( t_size(43)          /=  t_size(33))       stop 122
   if ( any(t_shape(43,:)   /=  t_shape(33,:)))   stop 123
   if ( any(t_lbound(43,:)  /=  t_lbound(33,:)))  stop 124
   if ( any(t_ubound(43,:)  /=  t_ubound(33,:)))  stop 125
   if ( t_size(44)          /=  t_size(34))       stop 126
   if ( any(t_shape(44,:)   /=  t_shape(34,:)))   stop 127
   if ( any(t_lbound(44,:)  /=  t_lbound(34,:)))  stop 128
   if ( any(t_ubound(44,:)  /=  t_ubound(34,:)))  stop 129
   if ( sum(C) /= 22026240 )  stop 130
   if ( sum(D) /= 48268800 )  stop 131
   end subroutine sub_mod
end module mod

program AllocatableDummyArgument101f
   use iso_c_binding
   use mod
   implicit none

   integer :: sumv, i, j, k
   integer :: t_size(50), t_shape(50,10), t_lbound(50,10), t_ubound(50,10)
   integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

   allocate (A(dim1, dim1))
   if (.not. allocated(A))  stop 1
   allocate (B(dim1, dim1))
   if (.not. allocated(B))  stop 2
   allocate (C(dim1, dim1))
   if (.not. allocated(C))  stop 3
   allocate (D(1,2,3,4,5,1,2,3,4,5))
   if (.not. allocated(D))  stop 4

   t_size = 0;  t_shape = 0;  t_lbound = 0;  t_ubound = 0;
   t_size(1)        = size(A)
   t_shape(1,1:2)   = shape(A)
   t_lbound(1,1:2)  = lbound(A)
   t_ubound(1,1:2)  = ubound(A)
   t_size(2)        = size(B)
   t_shape(2,1:2)   = shape(B)
   t_lbound(2,1:2)  = lbound(B)
   t_ubound(2,1:2)  = ubound(B)
   t_size(3)        = size(C)
   t_shape(3,1:2)   = shape(C)
   t_lbound(3,1:2)  = lbound(C)
   t_ubound(3,1:2)  = ubound(C)
   t_size(4)        = size(D)
   t_shape(4,1:10)  = shape(D)
   t_lbound(4,1:10) = lbound(D)
   t_ubound(4,1:10) = ubound(D)

!---------- Initializing arrays ----------
   sumv = 0
   do i = 1, dim1
     do j = 1, dim1
       A(i,j) = i + j
       B(i,j) = i - j
       sumv = sumv + ( A(i,j) + B(i,j) )
     end do
   end do
   if ( sumv /= 4352 )     stop 6

!---------- main program ----------
   C = 0; D = 0;
   do i = 1, dim1
      do j = 1, dim1
         do k = 1, dim1
            C = C + A(i,k)*B(k,j)
         end do
         D = D + A(i,j)
      end do
   end do
   if ( sum(C) /= 22282240 )  stop 31
   if ( sum(D) /= 62668800 )  stop 32

!---------- call BIND(C) procedure from main program
   call sub_bind_c(dim1, A, B, C, D, t_size, t_shape, t_lbound, t_ubound)

   if ( t_size(1)          /=  t_size(31))       stop 33
   if ( any(t_shape(1,:)   /=  t_shape(31,:)))   stop 34
   if ( any(t_lbound(1,:)  /=  t_lbound(31,:)))  stop 35
   if ( any(t_ubound(1,:)  /=  t_ubound(31,:)))  stop 36
   if ( t_size(2)          /=  t_size(32))       stop 37
   if ( any(t_shape(2,:)   /=  t_shape(32,:)))   stop 38
   if ( any(t_lbound(2,:)  /=  t_lbound(32,:)))  stop 39
   if ( any(t_ubound(2,:)  /=  t_ubound(32,:)))  stop 40
   if ( t_size(3)          /=  t_size(33))       stop 41
   if ( any(t_shape(3,:)   /=  t_shape(33,:)))   stop 42
   if ( any(t_lbound(3,:)  /=  t_lbound(33,:)))  stop 43
   if ( any(t_ubound(3,:)  /=  t_ubound(33,:)))  stop 44
   if ( t_size(4)          /=  t_size(34))       stop 45
   if ( any(t_shape(4,:)   /=  t_shape(34,:)))   stop 46
   if ( any(t_lbound(4,:)  /=  t_lbound(34,:)))  stop 47
   if ( any(t_ubound(4,:)  /=  t_ubound(34,:)))  stop 48
   if ( sum(C) /= 22026240 )  stop 49
   if ( sum(D) /= 48268800 )  stop 50

!---------- internal subprogram ----------
   call sub_int(t_size, t_shape, t_lbound, t_ubound)

!---------- module subprogram ----------
   call sub_mod(t_size, t_shape, t_lbound, t_ubound)

contains
  subroutine sub_int(t_size, t_shape, t_lbound, t_ubound)
     integer :: i, j, k
     integer :: t_size(50), t_shape(50,10), t_lbound(50,10), t_ubound(50,10)
     integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

     allocate (A(dim1, dim1))
     if (.not. allocated(A))  stop 51
     allocate (B(dim1, dim1))
     if (.not. allocated(B))  stop 52
     allocate (C(dim1, dim1))
     if (.not. allocated(C))  stop 53
     allocate (D(1,2,3,4,5,1,2,3,4,5))
     if (.not. allocated(D))  stop 54

     do i = 1, dim1
       do j = 1, dim1
         A(i,j) = i + j
         B(i,j) = i - j
       end do
     end do

     C = 100; D = 100;
     do i = 1, dim1
        do j = 1, dim1
           do k = 1, dim1
              C = C + A(i,k)*B(k,j)
           end do
        end do
           D = D + A(i,j)
     end do

     t_size(21)        = size(A)
     t_shape(21,1:2)   = shape(A)
     t_lbound(21,1:2)  = lbound(A)
     t_ubound(21,1:2)  = ubound(A)
     t_size(22)        = size(B)
     t_shape(22,1:2)   = shape(B)
     t_lbound(22,1:2)  = lbound(B)
     t_ubound(22,1:2)  = ubound(B)
     t_size(23)        = size(C)
     t_shape(23,1:2)   = shape(C)
     t_lbound(23,1:2)  = lbound(C)
     t_ubound(23,1:2)  = ubound(C)
     t_size(24)        = size(D)
     t_shape(24,1:10)  = shape(D)
     t_lbound(24,1:10) = lbound(D)
     t_ubound(24,1:10) = ubound(D)

!---------- call BIND(C) procedure from internal procedure
   call sub_bind_c(dim1, A, B, C, D, t_size, t_shape, t_lbound, t_ubound)

   if ( t_size(21)          /=  t_size(31))       stop 55
   if ( any(t_shape(21,:)   /=  t_shape(31,:)))   stop 56
   if ( any(t_lbound(21,:)  /=  t_lbound(31,:)))  stop 57
   if ( any(t_ubound(21,:)  /=  t_ubound(31,:)))  stop 58
   if ( t_size(22)          /=  t_size(32))       stop 59
   if ( any(t_shape(22,:)   /=  t_shape(32,:)))   stop 60
   if ( any(t_lbound(22,:)  /=  t_lbound(32,:)))  stop 61
   if ( any(t_ubound(22,:)  /=  t_ubound(32,:)))  stop 62
   if ( t_size(23)          /=  t_size(33))       stop 63
   if ( any(t_shape(23,:)   /=  t_shape(33,:)))   stop 64
   if ( any(t_lbound(23,:)  /=  t_lbound(33,:)))  stop 65
   if ( any(t_ubound(23,:)  /=  t_ubound(33,:)))  stop 66
   if ( t_size(24)          /=  t_size(34))       stop 67
   if ( any(t_shape(24,:)   /=  t_shape(34,:)))   stop 68
   if ( any(t_lbound(24,:)  /=  t_lbound(34,:)))  stop 69
   if ( any(t_ubound(24,:)  /=  t_ubound(34,:)))  stop 70
   if ( sum(C) /= 22026240 )  stop 71
   if ( sum(D) /= 48268800 )  stop 72
  end subroutine sub_int

end program AllocatableDummyArgument101f

  subroutine sub_bind_c(dim1, A, B, C, D, t_size, t_shape, t_lbound, t_ubound) bind(C)
     use iso_c_binding
     implicit none
     integer :: dim1
     integer :: i, j, k
     integer :: t_size(50), t_shape(50,10), t_lbound(50,10), t_ubound(50,10)
     integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

     if (.not. allocated(A))  stop 101
     if (.not. allocated(B))  stop 102
     if (.not. allocated(C))  stop 103
     if (.not. allocated(D))  stop 104

     C = -1000; D = -1000;
     do i = 1, dim1
        do j = 1, dim1
           do k = 1, dim1
              C = C + A(i,k)*B(k,j)
           end do
           D = D + A(i,j)
        end do
     end do

     t_size(31)        = size(A)
     t_shape(31,1:2)   = shape(A)
     t_lbound(31,1:2)  = lbound(A)
     t_ubound(31,1:2)  = ubound(A)
     t_size(32)        = size(B)
     t_shape(32,1:2)   = shape(B)
     t_lbound(32,1:2)  = lbound(B)
     t_ubound(32,1:2)  = ubound(B)
     t_size(33)        = size(C)
     t_shape(33,1:2)   = shape(C)
     t_lbound(33,1:2)  = lbound(C)
     t_ubound(33,1:2)  = ubound(C)
     t_size(34)        = size(D)
     t_shape(34,1:10)  = shape(D)
     t_lbound(34,1:10) = lbound(D)
     t_ubound(34,1:10) = ubound(D)
  end subroutine sub_bind_c
