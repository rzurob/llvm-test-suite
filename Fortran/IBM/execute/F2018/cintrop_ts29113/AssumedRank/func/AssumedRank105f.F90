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
     subroutine sub_bind_c_1(arr) bind(C)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_bind_c_1
     subroutine sub_bind_c_2(arr) bind(C)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_bind_c_2
     subroutine sub_bind_c_3(arr) bind(C)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_bind_c_3
     subroutine sub_bind_c_4(arr) bind(C)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_bind_c_4
     subroutine sub_1(arr)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_1
     subroutine sub_2(arr)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_2
     subroutine sub_3(arr)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_3
     subroutine sub_4(arr)
       import
       implicit none
       integer :: dim
       integer(c_int) :: arr(..)
     end subroutine sub_4
   end interface

   #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
   #else
      logical :: DEBUG_MODE = .FALSE.
   #endif

   contains
   subroutine sub_mod_1(arg)
     integer(c_int) :: arg(*)

     call sub_1(arg)
     call sub_bind_c_1(arg)
   end subroutine sub_mod_1

   subroutine sub_mod_2(arg)
     integer(c_int) :: arg(N,*)

     call sub_2(arg)
     call sub_bind_c_2(arg)
   end subroutine sub_mod_2

   subroutine sub_mod_3(arg)
     integer(c_int) :: arg(N,N,*)

     call sub_3(arg)
     call sub_bind_c_3(arg)
   end subroutine sub_mod_3

   subroutine sub_mod_4(arg)
     integer(c_int) :: arg(1,2,3,4,*)

     call sub_4(arg)
     call sub_bind_c_4(arg)
   end subroutine sub_mod_4
end module mod

program AssumedRank105f
   use iso_c_binding
   use mod
   implicit none

   integer :: sumv, i, j, k
   integer(c_int) :: A(N), B(-N+1:0,0:N-1), C(N,N,N), D(1,2,3,4,5,1,2,3,4,5)

   A = 0; B = 0; C = 0; D = 0;

!---------- call BIND(C)/Non-BIND(C) procedures from internal procedure
   call sub_int_1(A)
   call sub_int_2(B)
   call sub_int_3(C)
   call sub_int_4(D)

!---------- call BIND(C)/Non-BIND(C) procedures from module procedure
   call sub_mod_1(A)
   call sub_mod_2(B)
   call sub_mod_3(C)
   call sub_mod_4(D)

contains
   subroutine sub_int_1(arg)
     integer(c_int) :: arg(*)

     call sub_1(arg)
     call sub_bind_c_1(arg)
   end subroutine sub_int_1

   subroutine sub_int_2(arg)
     integer(c_int) :: arg(N,*)

     call sub_2(arg)
     call sub_bind_c_2(arg)
   end subroutine sub_int_2

   subroutine sub_int_3(arg)
     integer(c_int) :: arg(N,N,*)

     call sub_3(arg)
     call sub_bind_c_3(arg)
   end subroutine sub_int_3

   subroutine sub_int_4(arg)
     integer(c_int) :: arg(1,2,3,4,*)

     call sub_4(arg)
     call sub_bind_c_4(arg)
   end subroutine sub_int_4
end program AssumedRank105f

subroutine sub_bind_c_1(arr) bind(C)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is A"
     if (size(arr)          /=        -1) ERROR STOP 110
     if (any(shape(arr)     /=     [-1])) ERROR STOP 111
     if (rank(arr)          /=         1) ERROR STOP 112
     if (any(lbound(arr)    /=      [1])) ERROR STOP 113
     if (any(ubound(arr)    /=     [-1])) ERROR STOP 114
end subroutine sub_bind_c_1

subroutine sub_bind_c_2(arr) bind(C)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is B"
     if (size(arr)          /=           -10) ERROR STOP 120
     if (size(arr,1)        /=            10) ERROR STOP 121
     if (size(arr,2)        /=            -1) ERROR STOP 122
     if (any(shape(arr)     /=      [10,-1])) ERROR STOP 123
     if (rank(arr)          /=             2) ERROR STOP 124
     if (any(lbound(arr)    /=        [1,1])) ERROR STOP 125
     if (any(ubound(arr)    /=      [10,-1])) ERROR STOP 126
end subroutine sub_bind_c_2

subroutine sub_bind_c_3(arr) bind(C)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is C"
     if (size(arr)          /=          -100) ERROR STOP 130
     if (size(arr,1)        /=            10) ERROR STOP 131
     if (size(arr,2)        /=            10) ERROR STOP 132
     if (size(arr,3)        /=            -1) ERROR STOP 133
     if (any(shape(arr)     /=   [10,10,-1])) ERROR STOP 134
     if (rank(arr)          /=             3) ERROR STOP 135
     if (any(lbound(arr)    /=      [1,1,1])) ERROR STOP 136
     if (any(ubound(arr)    /=   [10,10,-1])) ERROR STOP 137
end subroutine sub_bind_c_3

subroutine sub_bind_c_4(arr) bind(C)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is D"
     if (size(arr)       /=                   -24) ERROR STOP 140
     if (any(shape(arr)  /=         [1,2,3,4,-1])) ERROR STOP 141
     if (rank(arr)       /=                     5) ERROR STOP 142
     if (any(lbound(arr) /=          [1,1,1,1,1])) ERROR STOP 143
     if (any(ubound(arr) /=         [1,2,3,4,-1])) ERROR STOP 144
end subroutine sub_bind_c_4

subroutine sub_1(arr)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is A"
     if (size(arr)          /=        -1) ERROR STOP 510
     if (any(shape(arr)     /=     [-1])) ERROR STOP 511
     if (rank(arr)          /=         1) ERROR STOP 512
     if (any(lbound(arr)    /=      [1])) ERROR STOP 513
     if (any(ubound(arr)    /=     [-1])) ERROR STOP 514
end subroutine sub_1

subroutine sub_2(arr)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is B"

     if (size(arr)          /=           -10) ERROR STOP 520
     if (size(arr,1)        /=            10) ERROR STOP 521
     if (size(arr,2)        /=            -1) ERROR STOP 522
     if (any(shape(arr)     /=      [10,-1])) ERROR STOP 523
     if (rank(arr)          /=             2) ERROR STOP 524
     if (any(lbound(arr)    /=        [1,1])) ERROR STOP 525
     if (any(ubound(arr)    /=      [10,-1])) ERROR STOP 526
end subroutine sub_2

subroutine sub_3(arr)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is C"
     if (size(arr)          /=          -100) ERROR STOP 530
     if (size(arr,1)        /=            10) ERROR STOP 531
     if (size(arr,2)        /=            10) ERROR STOP 532
     if (size(arr,3)        /=            -1) ERROR STOP 533
     if (any(shape(arr)     /=   [10,10,-1])) ERROR STOP 534
     if (rank(arr)          /=             3) ERROR STOP 535
     if (any(lbound(arr)    /=      [1,1,1])) ERROR STOP 536
     if (any(ubound(arr)    /=   [10,10,-1])) ERROR STOP 537
end subroutine sub_3

subroutine sub_4(arr)
     use iso_c_binding
     implicit none
     integer(c_int) :: arr(..)

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (DEBUG_MODE) then
        print*, "size:", size(arr)
        print*, "shape:", shape(arr)
        print*, "rank:", rank(arr)
        print*, "lower bound:", lbound(arr)
        print*, "upper bound:", ubound(arr)
     endif

     if (DEBUG_MODE) print*, "array is D"
     if (size(arr)       /=                   -24) ERROR STOP 540
     if (any(shape(arr)  /=         [1,2,3,4,-1])) ERROR STOP 541
     if (rank(arr)       /=                     5) ERROR STOP 542
     if (any(lbound(arr) /=          [1,1,1,1,1])) ERROR STOP 543
     if (any(ubound(arr) /=         [1,2,3,4,-1])) ERROR STOP 544
end subroutine sub_4
