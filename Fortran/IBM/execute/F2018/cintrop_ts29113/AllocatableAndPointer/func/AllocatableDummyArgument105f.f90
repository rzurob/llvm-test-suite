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
!*                                - type c_long_long
!*                                - LBOUND, UBOUND, SHAPE, SIZE, MAX
!*                                - Call to BIND(C) procedure from different scopes:
!*                                main program, module and internal procedure
!*                                - Nesting: call chain:
!*                                non-Bind(C) => Bind(C) => non-Bind(C)
!*                                module/internal/main => external => external
!*                                - Actual arg. is of type integer(8)
!*                                - Dummy arg. is of type integer(c_long_long)
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/D2/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
   implicit none
   integer, parameter   :: N = 16, M = 120

   interface
     subroutine sub_bind_c(N, M, A, B, C, D) bind(C)
       use iso_c_binding, only: c_long_long
       implicit none
       integer :: N, M
       integer(c_long_long), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)
     end subroutine sub_bind_c
   end interface

   contains
   subroutine sub_mod()
     use iso_c_binding, only: c_long_long
     integer :: i, j, k
     integer(8), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

     allocate (A(N, N))
     if (.not. allocated(A))  error stop 20
     allocate (B(N, N))
     if (.not. allocated(B))  error stop 21
     allocate (C(N, N))
     if (.not. allocated(C))  error stop 22
     allocate (D(1,2,3,4,5,1,2,3,4,5))
     if (.not. allocated(D))  error stop 23

     do i = 1, N
       do j = 1, N
         A(i,j) = i + j
         B(i,j) = i - j
       end do
     end do

     C = 0; D = 0;

     do i = 1, N
        do j = 1, N
           do k = 1, N
              C = C + A(i,k)*B(k,j)
           end do
           D = D + A(i,j)
        end do
     end do
     if ( maxval(C) /= 87040 )  error stop 24
     if ( maxval(D) /=  4352 )  error stop 25

!---------- call BIND(C) procedure from Module procedure
   call sub_bind_c(N, M, A, B, C, D)

   end subroutine sub_mod
end module mod

program AllocatableDummyArgument105f
   use iso_c_binding, only: c_long_long
   use mod
   implicit none

   integer :: sumv, i, j, k
   integer(8), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

   allocate (A(N, N))
   if (.not. allocated(A))  error stop 1
   allocate (B(N, N))
   if (.not. allocated(B))  error stop 2
   allocate (C(N, N))
   if (.not. allocated(C))  error stop 3
   allocate (D(1,2,3,4,5,1,2,3,4,5))
   if (.not. allocated(D))  error stop 4

!---------- Initializing arrays ----------
   sumv = 0
   do i = 1, N
     do j = 1, N
       A(i,j) = i + j
       B(i,j) = i - j
       sumv = sumv + ( A(i,j) + B(i,j) )
     end do
   end do
   if ( sumv /= 4352 )     error stop 5

!---------- main program ----------
   C = 0; D = 0;
   do i = 1, N
      do j = 1, N
         do k = 1, N
            C = C + A(i,k)*B(k,j)
         end do
         D = D + A(i,j)
      end do
   end do
   if ( maxval(C) /=   87040 )  error stop 6
   if ( maxval(D) /=    4352 )  error stop 7

!---------- call BIND(C) procedure from main program
   call sub_bind_c(N, M, A, B, C, D)

   if ( maxval(C) /= 86040 )  error stop 8
   if ( maxval(D) /=  3352 )  error stop 9

!---------- internal subprogram ----------
   call sub_int()

!---------- module subprogram ----------
   call sub_mod()

contains
  subroutine sub_int()
     integer :: i, j, k
     integer(c_long_long), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

     allocate (A(N, N))
     if (.not. allocated(A))  error stop 11
     allocate (B(N, N))
     if (.not. allocated(B))  error stop 12
     allocate (C(N, N))
     if (.not. allocated(C))  error stop 13
     allocate (D(1,2,3,4,5,1,2,3,4,5))
     if (.not. allocated(D))  error stop 14

     do i = 1, N
       do j = 1, N
         A(i,j) = i + j
         B(i,j) = i - j
       end do
     end do

     C = 0; D = 0;
     do i = 1, N
        do j = 1, N
           do k = 1, N
              C = C + A(i,k)*B(k,j)
           end do
           D = D + A(i,j)
        end do
     end do
     if ( maxval(C) /= 87040 )  error stop 15
     if ( maxval(D) /=  4352 )  error stop 16
!---------- call BIND(C) procedure from internal procedure
     call sub_bind_c(N, M, A, B, C, D)

     if ( maxval(C) /= 86040 )  error stop 17
     if ( maxval(D) /=  3352 )  error stop 18
  end subroutine sub_int
end program AllocatableDummyArgument105f

subroutine sub_bind_c(N, M, A, B, C, D) bind(C)
     use iso_c_binding, only: c_long_long
     implicit none
     integer :: N, M
     integer :: i, j, k
     integer(c_long_long), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

     interface
        subroutine sub_bind_c_ext(N, M, A1, B1, C1, D1)
           use iso_c_binding, only: c_long_long
           implicit none
           integer :: N, M
           integer :: i, j, k
           integer(8), allocatable :: A1(:,:), B1(:,:), C1(:,:), D1(:,:,:,:,:,:,:,:,:,:)
           integer(8), allocatable :: A2(:,:), B2(:,:), C2(:,:), D2(:,:,:,:,:,:,:,:,:,:)
        end subroutine sub_bind_c_ext
     end interface

     if (.not. allocated(A))  error stop 101
     if (.not. allocated(B))  error stop 102
     if (.not. allocated(C))  error stop 103
     if (.not. allocated(D))  error stop 104

     if ( size(A)          /=                             N*N ) error stop 105
     if ( size(B)          /=                             N*N ) error stop 106
     if ( size(C)          /=                             N*N ) error stop 107
     if ( size(D)          /=                             M*M ) error stop 108

     if ( any(shape(A)     /=                         [N, N]) ) error stop 109
     if ( any(shape(B)     /=                         [N, N]) ) error stop 110
     if ( any(shape(C)     /=                         [N, N]) ) error stop 111
     if ( any(shape(D)     /=       [(i, i=1,5), (i, i=1,5)]) ) error stop 112

     if ( any(lbound(A)    /=                        [1, 1]) ) error stop 113
     if ( any(lbound(B)    /=                        [1, 1]) ) error stop 114
     if ( any(lbound(C)    /=                        [1, 1]) ) error stop 115
     if ( any(lbound(D)    /=                 [(1, i=1,10)]) ) error stop 116

     if ( any(ubound(A)    /=                      [16, 16]) ) error stop 117
     if ( any(ubound(B)    /=                      [16, 16]) ) error stop 118
     if ( any(ubound(C)    /=                      [16, 16]) ) error stop 119
     if ( any(ubound(D)    /=      [(i, i=1,5), (i, i=1,5)]) ) error stop 120

     if ( maxval(A)           /=                           32 ) error stop 121
     if ( maxval(B)           /=                           15 ) error stop 122
     if ( maxval(C)           /=                        87040 ) error stop 123
     if ( maxval(D)           /=                         4352 ) error stop 124

     C = -1000; D = -1000;
     do i = 1, N
        do j = 1, N
           do k = 1, N
              C = C + A(i,k)*B(k,j)
           end do
           D = D + A(i,j)
        end do
     end do

     call sub_bind_c_ext(N, M, A, B, C, D)
end subroutine sub_bind_c

subroutine sub_bind_c_ext(N, M, A1, B1, C1, D1)
     use iso_c_binding, only: c_long_long
     implicit none
     integer :: N, M
     integer :: i, j, k
     integer(8), allocatable :: A1(:,:), B1(:,:), C1(:,:), D1(:,:,:,:,:,:,:,:,:,:)
     integer(8), allocatable :: A2(:,:), B2(:,:), C2(:,:), D2(:,:,:,:,:,:,:,:,:,:)

     if (.not. allocated(A1))  error stop 131
     if (.not. allocated(B1))  error stop 132
     if (.not. allocated(C1))  error stop 133
     if (.not. allocated(D1))  error stop 134

     if ( size(A1)          /=                            N*N ) error stop 135
     if ( size(B1)          /=                            N*N ) error stop 136
     if ( size(C1)          /=                            N*N ) error stop 137
     if ( size(D1)          /=                            M*M ) error stop 138

     if ( any(shape(A1)     /=                        [N, N]) ) error stop 139
     if ( any(shape(B1)     /=                        [N, N]) ) error stop 140
     if ( any(shape(C1)     /=                        [N, N]) ) error stop 141
     if ( any(shape(D1)     /=      [(i, i=1,5), (i, i=1,5)]) ) error stop 142

     if ( any(lbound(A1)    /=                        [1, 1]) ) error stop 143
     if ( any(lbound(B1)    /=                        [1, 1]) ) error stop 144
     if ( any(lbound(C1)    /=                        [1, 1]) ) error stop 145
     if ( any(lbound(D1)    /=                 [(1, i=1,10)]) ) error stop 146

     if ( any(ubound(A1)    /=                      [16, 16]) ) error stop 147
     if ( any(ubound(B1)    /=                      [16, 16]) ) error stop 148
     if ( any(ubound(C1)    /=                      [16, 16]) ) error stop 149
     if ( any(ubound(D1)    /=      [(i, i=1,5), (i, i=1,5)]) ) error stop 150

     if ( maxval(A1)           /=                          32 ) error stop 151
     if ( maxval(B1)           /=                          15 ) error stop 152
     if ( maxval(C1)           /=                       86040 ) error stop 153
     if ( maxval(D1)           /=                        3352 ) error stop 154

     ! source is actual argument
     allocate(A2, source=A1)
     allocate(B2, source=B1)
     allocate(C2, source=C1)
     allocate(D2, source=D1)

     if (.not. allocated(A2))  error stop 161
     if (.not. allocated(B2))  error stop 162
     if (.not. allocated(C2))  error stop 163
     if (.not. allocated(D2))  error stop 164

     if ( size(A2)          /=                             N*N ) error stop 169
     if ( size(B2)          /=                             N*N ) error stop 170
     if ( size(C2)          /=                             N*N ) error stop 171
     if ( size(D2)          /=                             M*M ) error stop 172

     if ( any(shape(A2)     /=                         [N, N]) ) error stop 173
     if ( any(shape(B2)     /=                         [N, N]) ) error stop 174
     if ( any(shape(C2)     /=                         [N, N]) ) error stop 175
     if ( any(shape(D2)     /=       [(i, i=1,5), (i, i=1,5)]) ) error stop 176

     if ( any(lbound(A2)    /=                        [1, 1]) ) error stop 177
     if ( any(lbound(B2)    /=                        [1, 1]) ) error stop 178
     if ( any(lbound(C2)    /=                        [1, 1]) ) error stop 179
     if ( any(lbound(D2)    /=                 [(1, i=1,10)]) ) error stop 180

     if ( any(ubound(A2)    /=                      [16, 16]) ) error stop 181
     if ( any(ubound(B2)    /=                      [16, 16]) ) error stop 182
     if ( any(ubound(C2)    /=                      [16, 16]) ) error stop 183
     if ( any(ubound(D2)    /=      [(i, i=1,5), (i, i=1,5)]) ) error stop 184

     if ( maxval(A2)           /=                          32 ) error stop 185
     if ( maxval(B2)           /=                          15 ) error stop 186
     if ( maxval(C2)           /=                       86040 ) error stop 187
     if ( maxval(D2)           /=                        3352 ) error stop 188

     deallocate(A2,B2,C2,D2)
end subroutine sub_bind_c_ext