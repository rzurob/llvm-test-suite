! *********************************************************************
!* ===================================================================
!*
!* DATE                         : August  25, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - type c_int
!*                                - Call to BIND(C) / Non-Bind(C) procedure from different scopes:
!*                                      main program, module and internal procedure
!*                                - Interface block appears in a module
!*                                - LBOUND, UBOUND, SHAPE, SIZE, RANK
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
   integer, parameter :: N = 10

   interface
     subroutine sub_bind_c(test, dim, arr) bind(C)
       use iso_c_binding
       implicit none
       integer :: test, dim
       integer(c_int) :: arr(..)
     end subroutine sub_bind_c
     subroutine sub_dim_bind_c(test, dim, arr) bind(C)
       use iso_c_binding
       implicit none
       integer :: test, dim
       integer(c_int), dimension(..) :: arr
     end subroutine sub_dim_bind_c
     subroutine sub(test, dim, arr)
       use iso_c_binding
       implicit none
       integer :: test, dim
       integer(c_int) :: arr(..)
     end subroutine sub
     subroutine sub_dim(test, dim, arr)
       use iso_c_binding
       implicit none
       integer :: test, dim
       integer(c_int), dimension(..) :: arr
     end subroutine sub_dim
   end interface

   #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
   #else
      logical :: DEBUG_MODE = .FALSE.
   #endif

   contains
   subroutine sub_mod()
     use iso_c_binding
     integer :: i, j, k
     integer(c_int) :: AAA(N), BBB(-N+1:0,0:N-1), CCC(N,N,N), DDD(1,2,3,4,5,1,2,3,4,5)

     AAA = 0; BBB = 0; CCC = 0; DDD = 0;

!---------- call Non-BIND(C) procedure from Module procedure
   call sub(1, N, AAA(2:N-1))
   call sub(2, N, BBB(-N+2:-1,1:N-2))
   call sub(3, N, CCC(2:3,2:4,2:5))
   call sub(4, N, DDD(1,2,3,4,5,1,2,3,4,:))
   call sub_dim(1, N, AAA(2:N-1))
   call sub_dim(2, N, BBB(-N+2:-1,1:N-2))
   call sub_dim(3, N, CCC(2:3,2:4,2:5))
   call sub_dim(4, N, DDD(1,2,3,4,5,1,2,3,4,:))

!---------- call BIND(C) procedure from Module procedure
   call sub_bind_c(1, N, AAA(2:N-1))
   call sub_bind_c(2, N, BBB(-N+2:-1,1:N-2))
   call sub_bind_c(3, N, CCC(2:3,2:4,2:5))
   call sub_bind_c(4, N, DDD(1,2,3,4,5,1,2,3,4,:))
   call sub_dim_bind_c(1, N, AAA(2:N-1))
   call sub_dim_bind_c(2, N, BBB(-N+2:-1,1:N-2))
   call sub_dim_bind_c(3, N, CCC(2:3,2:4,2:5))
   call sub_dim_bind_c(4, N, DDD(1,2,3,4,5,1,2,3,4,:))

   end subroutine sub_mod
end module mod

program AssumedRank104f
   use iso_c_binding
   use mod
   implicit none

   integer :: sumv, i, j, k
   integer(c_int) :: A(N), B(-N+1:0,0:N-1), C(N,N,N), D(1,2,3,4,5,1,2,3,4,5)

   A = 0; B = 0; C = 0; D = 0;

!---------- call Non-BIND(C) procedure from Module procedure
   call sub(1, N, A(2:N-1))
   call sub(2, N, B(-N+2:-1,1:N-2))
   call sub(3, N, C(2:3,2:4,2:5))
   call sub(4, N, D(1,2,3,4,5,1,2,3,4,:))
   call sub_dim(1, N, A(2:N-1))
   call sub_dim(2, N, B(-N+2:-1,1:N-2))
   call sub_dim(3, N, C(2:3,2:4,2:5))
   call sub_dim(4, N, D(1,2,3,4,5,1,2,3,4,:))

!---------- call BIND(C) procedure from Module procedure
   call sub_bind_c(1, N, A(2:N-1))
   call sub_bind_c(2, N, B(-N+2:-1,1:N-2))
   call sub_bind_c(3, N, C(2:3,2:4,2:5))
   call sub_bind_c(4, N, D(1,2,3,4,5,1,2,3,4,:))
   call sub_dim_bind_c(1, N, A(2:N-1))
   call sub_dim_bind_c(2, N, B(-N+2:-1,1:N-2))
   call sub_dim_bind_c(3, N, C(2:3,2:4,2:5))
   call sub_dim_bind_c(4, N, D(1,2,3,4,5,1,2,3,4,:))

!---------- internal subprogram ----------
   call sub_int()

!---------- module subprogram ----------
   call sub_mod()

contains
  subroutine sub_int()
     integer :: i, j, k
     integer(c_int) :: AA(N), BB(-N+1:0,0:N-1), CC(N,N,N), DD(1,2,3,4,5,1,2,3,4,5)

     AA = 0; BB = 0; CC = 0; DD = 0;

!---------- call Non-BIND(C) procedure from Module procedure
   call sub(1, N, AA(2:N-1))
   call sub(2, N, BB(-N+2:-1,1:N-2))
   call sub(3, N, CC(2:3,2:4,2:5))
   call sub(4, N, DD(1,2,3,4,5,1,2,3,4,:))
   call sub_dim(1, N, AA(2:N-1))
   call sub_dim(2, N, BB(-N+2:-1,1:N-2))
   call sub_dim(3, N, CC(2:3,2:4,2:5))
   call sub_dim(4, N, DD(1,2,3,4,5,1,2,3,4,:))

!---------- call BIND(C) procedure from Module procedure
   call sub_bind_c(1, N, AA(2:N-1))
   call sub_bind_c(2, N, BB(-N+2:-1,1:N-2))
   call sub_bind_c(3, N, CC(2:3,2:4,2:5))
   call sub_bind_c(4, N, DD(1,2,3,4,5,1,2,3,4,:))
   call sub_dim_bind_c(1, N, AA(2:N-1))
   call sub_dim_bind_c(2, N, BB(-N+2:-1,1:N-2))
   call sub_dim_bind_c(3, N, CC(2:3,2:4,2:5))
   call sub_dim_bind_c(4, N, DD(1,2,3,4,5,1,2,3,4,:))
  end subroutine sub_int
end program AssumedRank104f

subroutine sub_bind_c(test, dim, arr) bind(C)
     use iso_c_binding
     implicit none
     integer :: test, dim, N
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "flag:", test
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

! verification part. depends on "dim" and "test"
     if (test == 1) then
         if (DEBUG_MODE) print*, "array is A"
         if (size(arr)          /=         N-2) ERROR STOP 110
         if (any(shape(arr)     /=      [N-2])) ERROR STOP 111
         if (rank(arr)          /=           1) ERROR STOP 112
         if (any(lbound(arr)    /=        [1])) ERROR STOP 113
         if (any(ubound(arr)    /=      [N-2])) ERROR STOP 114
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "array is B"
         if (size(arr)          /=      (N-2)**2) ERROR STOP 120
         if (any(shape(arr)     /=    [N-2,N-2])) ERROR STOP 121
         if (rank(arr)          /=             2) ERROR STOP 122
         if (any(lbound(arr)    /=        [1,1])) ERROR STOP 123
         if (any(ubound(arr)    /=    [N-2,N-2])) ERROR STOP 124
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "array is C"
         if (size(arr)          /=         2*3*4) ERROR STOP 130
         if (any(shape(arr)     /=      [2,3,4])) ERROR STOP 131
         if (rank(arr)          /=             3) ERROR STOP 132
         if (any(lbound(arr)    /=      [1,1,1])) ERROR STOP 133
         if (any(ubound(arr)    /=      [2,3,4])) ERROR STOP 134
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "array is D"
         if (size(arr)       /=     5) ERROR STOP 140
         if (any(shape(arr)  /=  [5])) ERROR STOP 141
         if (rank(arr)       /=     1) ERROR STOP 142
         if (any(lbound(arr) /=  [1])) ERROR STOP 143
         if (any(ubound(arr) /=  [5])) ERROR STOP 144
     else
         print*, "Error: this case should not exist"
         ERROR STOP 100
     endif
end subroutine sub_bind_c

subroutine sub(test, dim, arr)
     use iso_c_binding
     implicit none
     integer :: test, dim, N
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "flag:", test
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

! verification part. depends on "dim" and "test"
     if (test == 1) then
         if (DEBUG_MODE) print*, "array is A"
         if (size(arr)          /=         N-2) ERROR STOP 210
         if (any(shape(arr)     /=      [N-2])) ERROR STOP 211
         if (rank(arr)          /=           1) ERROR STOP 212
         if (any(lbound(arr)    /=        [1])) ERROR STOP 213
         if (any(ubound(arr)    /=      [N-2])) ERROR STOP 214
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "array is B"
         if (size(arr)          /=      (N-2)**2) ERROR STOP 220
         if (any(shape(arr)     /=    [N-2,N-2])) ERROR STOP 221
         if (rank(arr)          /=             2) ERROR STOP 222
         if (any(lbound(arr)    /=        [1,1])) ERROR STOP 223
         if (any(ubound(arr)    /=    [N-2,N-2])) ERROR STOP 224
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "array is C"
         if (size(arr)          /=         2*3*4) ERROR STOP 230
         if (any(shape(arr)     /=      [2,3,4])) ERROR STOP 231
         if (rank(arr)          /=             3) ERROR STOP 232
         if (any(lbound(arr)    /=      [1,1,1])) ERROR STOP 233
         if (any(ubound(arr)    /=      [2,3,4])) ERROR STOP 234
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "array is D"
         if (size(arr)       /=     5) ERROR STOP 240
         if (any(shape(arr)  /=  [5])) ERROR STOP 241
         if (rank(arr)       /=     1) ERROR STOP 242
         if (any(lbound(arr) /=  [1])) ERROR STOP 243
         if (any(ubound(arr) /=  [5])) ERROR STOP 244
     else
         print*, "Error: this case should not exist"
         ERROR STOP 200
     endif
end subroutine sub

subroutine sub_dim_bind_c(test, dim, arr) bind(C)
     use iso_c_binding
     implicit none
     integer :: test, dim, N
     integer(c_int), dimension(..) :: arr

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "flag:", test
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

! verification part. depends on "dim" and "test"
     if (test == 1) then
         if (DEBUG_MODE) print*, "array is A"
         if (size(arr)          /=         N-2) ERROR STOP 310
         if (any(shape(arr)     /=      [N-2])) ERROR STOP 311
         if (rank(arr)          /=           1) ERROR STOP 312
         if (any(lbound(arr)    /=        [1])) ERROR STOP 313
         if (any(ubound(arr)    /=      [N-2])) ERROR STOP 314
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "array is B"
         if (size(arr)          /=      (N-2)**2) ERROR STOP 320
         if (any(shape(arr)     /=    [N-2,N-2])) ERROR STOP 321
         if (rank(arr)          /=             2) ERROR STOP 322
         if (any(lbound(arr)    /=        [1,1])) ERROR STOP 323
         if (any(ubound(arr)    /=    [N-2,N-2])) ERROR STOP 324
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "array is C"
         if (size(arr)          /=         2*3*4) ERROR STOP 330
         if (any(shape(arr)     /=      [2,3,4])) ERROR STOP 331
         if (rank(arr)          /=             3) ERROR STOP 332
         if (any(lbound(arr)    /=      [1,1,1])) ERROR STOP 333
         if (any(ubound(arr)    /=      [2,3,4])) ERROR STOP 334
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "array is D"
         if (size(arr)       /=     5) ERROR STOP 340
         if (any(shape(arr)  /=  [5])) ERROR STOP 341
         if (rank(arr)       /=     1) ERROR STOP 342
         if (any(lbound(arr) /=  [1])) ERROR STOP 343
         if (any(ubound(arr) /=  [5])) ERROR STOP 344
     else
         print*, "Error: this case should not exist"
         ERROR STOP 300
     endif
end subroutine sub_dim_bind_c

subroutine sub_dim(test, dim, arr)
     use iso_c_binding
     implicit none
     integer :: test, dim, N
     integer(c_int), dimension(..) :: arr

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "flag:", test
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

! verification part. depends on "dim" and "test"
     if (test == 1) then
         if (DEBUG_MODE) print*, "array is A"
         if (size(arr)          /=         N-2) ERROR STOP 410
         if (any(shape(arr)     /=      [N-2])) ERROR STOP 411
         if (rank(arr)          /=           1) ERROR STOP 412
         if (any(lbound(arr)    /=        [1])) ERROR STOP 413
         if (any(ubound(arr)    /=      [N-2])) ERROR STOP 414
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "array is B"
         if (size(arr)          /=      (N-2)**2) ERROR STOP 420
         if (any(shape(arr)     /=    [N-2,N-2])) ERROR STOP 421
         if (rank(arr)          /=             2) ERROR STOP 422
         if (any(lbound(arr)    /=        [1,1])) ERROR STOP 423
         if (any(ubound(arr)    /=    [N-2,N-2])) ERROR STOP 424
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "array is C"
         if (size(arr)          /=         2*3*4) ERROR STOP 430
         if (any(shape(arr)     /=      [2,3,4])) ERROR STOP 431
         if (rank(arr)          /=             3) ERROR STOP 432
         if (any(lbound(arr)    /=      [1,1,1])) ERROR STOP 433
         if (any(ubound(arr)    /=      [2,3,4])) ERROR STOP 434
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "array is D"
         if (size(arr)       /=     5) ERROR STOP 440
         if (any(shape(arr)  /=  [5])) ERROR STOP 441
         if (rank(arr)       /=     1) ERROR STOP 442
         if (any(lbound(arr) /=  [1])) ERROR STOP 443
         if (any(ubound(arr) /=  [5])) ERROR STOP 444
     else
         print*, "Error: this case should not exist"
         ERROR STOP 400
     endif
end subroutine sub_dim
