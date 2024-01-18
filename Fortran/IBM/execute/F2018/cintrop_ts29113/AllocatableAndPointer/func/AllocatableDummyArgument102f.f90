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
!*                                - type c_int
!*                                - LBOUND, UBOUND, SHAPE, SIZE, SUM
!*                                - INTENT(INOUT)
!*                                - Call to BIND(C) procedure from different scopes:
!*                                main program, module and internal procedure
!*                                - Nesting: call chain:
!*                                non-Bind(C) => Bind(C) => non-Bind(C)
!*                                module/internal/main => external => internal
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
       use iso_c_binding
       implicit none
       integer :: N, M
       integer(c_int), allocatable, intent(inout) :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)
     end subroutine sub_bind_c
   end interface

   contains
   subroutine sub_mod()
     use iso_c_binding
     integer :: i, j, k
     integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

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
     if ( sum(C) /= 22282240 )  error stop 24
     if ( sum(D) /= 62668800 )  error stop 25

!---------- call BIND(C) procedure from Module procedure
   call sub_bind_c(N, M, A, B, C, D)

   end subroutine sub_mod
end module mod

program AllocatableDummyArgument102f
   use iso_c_binding
   use mod
   implicit none

   integer :: sumv, i, j, k
   integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

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
   if ( sum(C) /= 22282240 )  error stop 6
   if ( sum(D) /= 62668800 )  error stop 7

!---------- call BIND(C) procedure from main program
   call sub_bind_c(N, M, A, B, C, D)

   if ( sum(C) /= 22026240 )  error stop 8
   if ( sum(D) /= 48268800 )  error stop 9

!---------- internal subprogram ----------
   call sub_int()

!---------- module subprogram ----------
   call sub_mod()

contains
  subroutine sub_int()
     integer :: i, j, k
     integer(c_int), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

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
     if ( sum(C) /= 22282240 )  error stop 15
     if ( sum(D) /= 62668800 )  error stop 16
!---------- call BIND(C) procedure from internal procedure
     call sub_bind_c(N, M, A, B, C, D)

     if ( sum(C) /= 22026240 )  error stop 17
     if ( sum(D) /= 48268800 )  error stop 18
  end subroutine sub_int
end program AllocatableDummyArgument102f

subroutine sub_bind_c(N, M, A, B, C, D) bind(c)
     use iso_c_binding
     implicit none
     integer :: N, M
     integer :: i, j, k
     integer(c_int), allocatable, intent(inout) :: A(:,:), B(:,:), C(:,:), D(:,:,:,:,:,:,:,:,:,:)

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

     if ( sum(A)           /=                           4352 ) error stop 121
     if ( sum(B)           /=                              0 ) error stop 122
     if ( sum(C)           /=                       22282240 ) error stop 123
     if ( sum(D)           /=                       62668800 ) error stop 124

     C = -1000; D = -1000;
     do i = 1, N
        do j = 1, N
           do k = 1, N
              C = C + A(i,k)*B(k,j)
           end do
           D = D + A(i,j)
        end do
     end do

     call sub_bind_c_int(A, B, C, D)

contains
  subroutine sub_bind_c_int(A1, B1, C1, D1)
     integer(c_int), allocatable :: A1(:,:), B1(:,:), C1(:,:), D1(:,:,:,:,:,:,:,:,:,:)
     integer(c_int), allocatable :: A2(:,:), B2(:,:), C2(:,:), D2(:,:,:,:,:,:,:,:,:,:)

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

     if ( sum(A1)           /=                           4352 ) error stop 151
     if ( sum(B1)           /=                              0 ) error stop 152
     if ( sum(C1)           /=                       22026240 ) error stop 153
     if ( sum(D1)           /=                       48268800 ) error stop 154

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

     if ( sum(A2)           /=                           4352 ) error stop 185
     if ( sum(B2)           /=                              0 ) error stop 186
     if ( sum(C2)           /=                       22026240 ) error stop 187
     if ( sum(D2)           /=                       48268800 ) error stop 188

     deallocate(A2,B2,C2,D2)

     ! source is host associated
     allocate(A2, source=A)
     allocate(B2, source=B)
     allocate(C2, source=C)
     allocate(D2, source=D)

     if (.not. allocated(A2))  error stop 191
     if (.not. allocated(B2))  error stop 192
     if (.not. allocated(C2))  error stop 193
     if (.not. allocated(D2))  error stop 194

     if ( size(A2)          /=                             N*N ) error stop 195
     if ( size(B2)          /=                             N*N ) error stop 196
     if ( size(C2)          /=                             N*N ) error stop 197
     if ( size(D2)          /=                             M*M ) error stop 198

     if ( any(shape(A2)     /=                         [N, N]) ) error stop 199
     if ( any(shape(B2)     /=                         [N, N]) ) error stop 200
     if ( any(shape(C2)     /=                         [N, N]) ) error stop 201
     if ( any(shape(D2)     /=       [(i, i=1,5), (i, i=1,5)]) ) error stop 202

     if ( any(lbound(A2)    /=                        [1, 1]) ) error stop 203
     if ( any(lbound(B2)    /=                        [1, 1]) ) error stop 204
     if ( any(lbound(C2)    /=                        [1, 1]) ) error stop 205
     if ( any(lbound(D2)    /=                 [(1, i=1,10)]) ) error stop 206

     if ( any(ubound(A2)    /=                      [16, 16]) ) error stop 207
     if ( any(ubound(B2)    /=                      [16, 16]) ) error stop 208
     if ( any(ubound(C2)    /=                      [16, 16]) ) error stop 209
     if ( any(ubound(D2)    /=      [(i, i=1,5), (i, i=1,5)]) ) error stop 210

     if ( sum(A2)           /=                           4352 ) error stop 211
     if ( sum(B2)           /=                              0 ) error stop 212
     if ( sum(C2)           /=                       22026240 ) error stop 213
     if ( sum(D2)           /=                       48268800 ) error stop 214

  end subroutine sub_bind_c_int
end subroutine sub_bind_c
