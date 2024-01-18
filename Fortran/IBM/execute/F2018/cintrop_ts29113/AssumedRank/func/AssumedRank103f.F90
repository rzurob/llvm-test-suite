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
!*                                - Actual arg is assumed size array
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
       use iso_c_binding
   implicit none
   integer, parameter :: N = 10

   interface
     subroutine sub_bind_c(dim, arr) bind(C)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_bind_c
     subroutine sub_dim_bind_c(dim, arr) bind(C)
       import
       implicit none
       integer :: dim
       integer(c_int), dimension(..) :: arr
     end subroutine sub_dim_bind_c
     subroutine sub(dim, arr)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub
     subroutine sub_dim(dim, arr)
       import
       implicit none
       integer :: dim
       integer(c_int), dimension(..) :: arr
     end subroutine sub_dim
   end interface

   #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
   #else
      logical :: DEBUG_MODE = .FALSE.
   #endif

   contains
   subroutine sub_mod(arg, flag)
     integer :: flag
     integer(c_int) :: arg(*)

!---------- call Non-BIND(C) procedure from Module procedure
   call sub(N, arg)
   call sub(N, arg)
   call sub(N, arg)
   call sub(N, arg)
   call sub_dim(N, arg)
   call sub_dim(N, arg)
   call sub_dim(N, arg)
   call sub_dim(N, arg)

!---------- call BIND(C) procedure from Module procedure
   call sub_bind_c(N, arg)
   call sub_bind_c(N, arg)
   call sub_bind_c(N, arg)
   call sub_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)

   end subroutine sub_mod
end module mod

program AssumedRank103f
   use iso_c_binding
   use mod
   implicit none

   integer :: sumv, i, j, k
   integer(c_int) :: A(N), B(-N+1:0,0:N-1), C(N,N,N), D(1,2,3,4,5,1,2,3,4,5)

   A = 0; B = 0; C = 0; D = 0;

!---------- call BIND(C)/Non-BIND(C) procedures from internal procedure
   call sub_int(A, 1)
   call sub_int(B, 2)
   call sub_int(C, 3)
   call sub_int(D, 4)

!---------- call BIND(C)/Non-BIND(C) procedures from module procedure
   call sub_mod(A, 1)
   call sub_mod(B, 1)
   call sub_mod(C, 1)
   call sub_mod(D, 1)

contains
  subroutine sub_int(arg, flag)
     integer :: flag
     integer(c_int) :: arg(*)

!---------- call Non-BIND(C) procedure from Internal procedure
   call sub(N, arg)
   call sub(N, arg)
   call sub(N, arg)
   call sub(N, arg)
   call sub_dim(N, arg)
   call sub_dim(N, arg)
   call sub_dim(N, arg)
   call sub_dim(N, arg)

!---------- call BIND(C) procedure from Internal procedure
   call sub_bind_c(N, arg)
   call sub_bind_c(N, arg)
   call sub_bind_c(N, arg)
   call sub_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)
   call sub_dim_bind_c(N, arg)

  end subroutine sub_int
end program AssumedRank103f

subroutine sub_bind_c(dim, arr) bind(C)
     use iso_c_binding
     implicit none
     integer :: dim, N
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (size(arr)          /=        -1) ERROR STOP 110
     if (any(shape(arr)     /=     [-1])) ERROR STOP 111
     if (rank(arr)          /=         1) ERROR STOP 112
     if (any(lbound(arr)    /=      [1])) ERROR STOP 113
     if (any(ubound(arr)    /=     [-1])) ERROR STOP 114
end subroutine sub_bind_c

subroutine sub(dim, arr)
     use iso_c_binding
     implicit none
     integer :: dim, N
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (size(arr)          /=        -1) ERROR STOP 210
     if (any(shape(arr)     /=     [-1])) ERROR STOP 211
     if (rank(arr)          /=         1) ERROR STOP 212
     if (any(lbound(arr)    /=      [1])) ERROR STOP 213
     if (any(ubound(arr)    /=     [-1])) ERROR STOP 214
end subroutine sub

subroutine sub_dim_bind_c(dim, arr) bind(C)
     use iso_c_binding
     implicit none
     integer :: dim, N
     integer(c_int), dimension(..) :: arr

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

    if (size(arr)          /=        -1) ERROR STOP 310
    if (any(shape(arr)     /=     [-1])) ERROR STOP 311
    if (rank(arr)          /=         1) ERROR STOP 312
    if (any(lbound(arr)    /=      [1])) ERROR STOP 313
    if (any(ubound(arr)    /=     [-1])) ERROR STOP 314
end subroutine sub_dim_bind_c

subroutine sub_dim(dim, arr)
     use iso_c_binding
     implicit none
     integer :: dim, N
     integer(c_int), dimension(..) :: arr

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     N = dim
     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (size(arr)          /=        -1) ERROR STOP 410
     if (any(shape(arr)     /=     [-1])) ERROR STOP 411
     if (rank(arr)          /=         1) ERROR STOP 412
     if (any(lbound(arr)    /=      [1])) ERROR STOP 413
     if (any(ubound(arr)    /=     [-1])) ERROR STOP 414
end subroutine sub_dim
