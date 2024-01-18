! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedRank110f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : August  25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank object
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - Call to BIND(C) procedure from different scopes:
!*                                main program, external and internal procedure
!*                                - Interface block appears in the main program 
!*                                - LBOUND, UBOUND, SHAPE, SIZE, RANK 
!*                                - Allocatable dummy Arg. of a BIND(C) procedure
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedRank110f 
   implicit none

   integer, parameter   :: dim1 = 16
   integer :: i, j, k
   integer, allocatable :: sumv, A(:), B(:,:), C(:,:,:), D(:,:,:,:,:,:,:,:,:,:)

   interface
     subroutine sub_bind_c(arr, test) bind(C)
         implicit none
         integer :: test
         integer, allocatable :: arr(..) 
     end subroutine sub_bind_c
     subroutine sub(arr, test)
         implicit none
         integer :: test
         integer, allocatable :: arr(..) 
     end subroutine sub
     subroutine sub_ext(al, A, B, C, D)
         integer, allocatable :: al, A(:), B(:,:), C(:,:,:), D(:,:,:,:,:,:,:,:,:,:)
     end subroutine sub_ext
   end interface

   #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
   #else
      logical :: DEBUG_MODE = .FALSE.
   #endif

   allocate (A(dim1))
   if (.not. allocated(A))  ERROR STOP 1
   allocate (B(dim1, dim1))
   if (.not. allocated(B))  ERROR STOP 2
   allocate (C(dim1, dim1, dim1))
   if (.not. allocated(C))  ERROR STOP 3
   allocate (D(1,2,3,4,5,1,2,3,4,5))
   if (.not. allocated(D))  ERROR STOP 4

   A = 0; B = 0; C = 0; D = 0;
   do i = 1, dim1
     do j = 1, dim1
       do k = 1, dim1
           A(i) = k - j + i
           B(i,j) = i + j
           C(i,j,k) = A(i) + B(j,k)
           sumv = A(i) + B(i,j) + C(i,j,k)
       end do
     end do
   end do
   if (.not. allocated(sumv))  ERROR STOP 5

!---------- call BIND(C) procedure from main program
   call sub_bind_c(sumv, 0)
   call sub_bind_c(A, 1)
   call sub_bind_c(B, 2)
   call sub_bind_c(C, 3)
   call sub_bind_c(D, 4)

!---------- call non-BIND(C) procedure from main program
   call sub(sumv, 0)
   call sub(A, 1)
   call sub(B, 2)
   call sub(C, 3)
   call sub(D, 4)

!---------- internal subprogram ----------
   call sub_int(sumv, A, B, C, D)

!---------- external subprogram ----------
   call sub_ext(sumv, A, B, C, D)

contains
  subroutine sub_int(al, A, B, C, D)
     integer, allocatable :: al, A(:), B(:,:), C(:,:,:), D(:,:,:,:,:,:,:,:,:,:)

     if (.not. allocated(al))  ERROR STOP 10
     if (.not.  allocated(A))  ERROR STOP 11
     if (.not.  allocated(B))  ERROR STOP 12
     if (.not.  allocated(C))  ERROR STOP 13
     if (.not.  allocated(D))  ERROR STOP 14

!---------- call BIND(C) procedure from internal procedure 
     call sub_bind_c(al, 0)
     call sub_bind_c(A, 1)
     call sub_bind_c(B, 2)
     call sub_bind_c(C, 3)
     call sub_bind_c(D, 4)
!---------- call non-BIND(C) procedure from main program
     call sub(al, 0)
     call sub(A, 1)
     call sub(B, 2)
     call sub(C, 3)
     call sub(D, 4)
  end subroutine sub_int
end program AssumedRank110f

subroutine sub_ext(al, A, B, C, D)
     implicit none
     integer, allocatable :: al, A(:), B(:,:), C(:,:,:), D(:,:,:,:,:,:,:,:,:,:)

     interface
       subroutine sub_bind_c(arr, test) bind(C)
           implicit none
           integer :: test
           integer, allocatable :: arr(..) 
       end subroutine sub_bind_c
       subroutine sub(arr, test)
           implicit none
           integer :: test
           integer, allocatable :: arr(..) 
       end subroutine sub
     end interface

     if (.not. allocated(al))  ERROR STOP 20
     if (.not.  allocated(A))  ERROR STOP 21
     if (.not.  allocated(B))  ERROR STOP 22
     if (.not.  allocated(C))  ERROR STOP 23
     if (.not.  allocated(D))  ERROR STOP 24

!---------- call BIND(C) procedure from external procedure 
     call sub_bind_c(al, 0)
     call sub_bind_c(A, 1)
     call sub_bind_c(B, 2)
     call sub_bind_c(C, 3)
     call sub_bind_c(D, 4)
!---------- call non-BIND(C) procedure from main program
     call sub(al, 0)
     call sub(A, 1)
     call sub(B, 2)
     call sub(C, 3)
     call sub(D, 4)
end subroutine sub_ext

subroutine sub_bind_c(arr, test) bind(C)
     implicit none
     ! Dummy arg. 
     integer :: test
     integer, allocatable :: arr(..) 
     ! Internal variables 
     integer, parameter :: dim1 = 16

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (.not. allocated(arr))  ERROR STOP 101
     if (DEBUG_MODE) then 
        print*, test 
        print*, size(arr)
        print*, shape(arr)
        print*, rank(arr)
        print*, lbound(arr)
        print*, ubound(arr)
     endif

! verification part. Depends on the argument and on the value test has     
     if (test == 0) then
         if (DEBUG_MODE) print*, "scalar dummy arg."
         if (size(arr)      /=            1) ERROR STOP 110
         if (rank(arr)      /=            0) ERROR STOP 111
     elseif (test == 1) then
         if (DEBUG_MODE) print*, "array is A"
         if (size(arr)      /=         dim1) ERROR STOP 112
         if (any(shape(arr) /=    [dim1])) ERROR STOP 113
         if (rank(arr)      /=            1) ERROR STOP 114
         if (any(lbound(arr)   /=    [1])) ERROR STOP 115
         if (any(ubound(arr)   /= [dim1])) ERROR STOP 116
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "array is B"
         if (size(arr)      /=    dim1*dim1) ERROR STOP 120
         if (any(shape(arr) /= [dim1,dim1])) ERROR STOP 121
         if (rank(arr)      /=            2) ERROR STOP 122
         if (any(lbound(arr)    /=        [1,1])) ERROR STOP 123
         if (any(ubound(arr)    /=  [dim1,dim1])) ERROR STOP 124
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "array is C"
         if (size(arr)      /=    dim1*dim1*dim1) ERROR STOP 130
         if (any(shape(arr) /= [dim1,dim1,dim1])) ERROR STOP 131
         if (rank(arr)      /=            3) ERROR STOP 132
         if (any(lbound(arr)    /=        [1,1,1])) ERROR STOP 133
         if (any(ubound(arr)    /=  [dim1,dim1,dim1])) ERROR STOP 134
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "array is D"
         if (size(arr)      /=                  14400) ERROR STOP 140
         if (any(shape(arr) /= [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 141
         if (rank(arr)      /=                     10) ERROR STOP 142
         if (any(lbound(arr)    /=  [1,1,1,1,1,1,1,1,1,1])) ERROR STOP 143
         if (any(ubound(arr)    /=  [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 144
     else
         print*, "Error: this case should not exist"
         ERROR STOP 100
     endif
end subroutine sub_bind_c

subroutine sub(arr, test)
     implicit none
     ! Dummy arg. 
     integer :: test
     integer, allocatable :: arr(..) 
     ! Internal variables 
     integer, parameter :: dim1 = 16

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (.not. allocated(arr))  ERROR STOP 201
     if (DEBUG_MODE) then 
        print*, test 
        print*, size(arr)
        print*, shape(arr)
        print*, rank(arr)
        print*, lbound(arr)
        print*, ubound(arr)
     endif

! verification part. Depends on the argument and on the value test has     
     if (test == 0) then
         if (DEBUG_MODE) print*, "scalar dummy arg."
         if (size(arr)      /=            1) ERROR STOP 210
         if (rank(arr)      /=            0) ERROR STOP 211
     elseif (test == 1) then
         if (DEBUG_MODE) print*, "array is A"
         if (size(arr)      /=         dim1) ERROR STOP 212
         if (any(shape(arr) /=    [dim1])) ERROR STOP 213
         if (rank(arr)      /=            1) ERROR STOP 214
         if (any(lbound(arr)   /=    [1])) ERROR STOP 215
         if (any(ubound(arr)   /= [dim1])) ERROR STOP 216
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "array is B"
         if (size(arr)      /=    dim1*dim1) ERROR STOP 220
         if (any(shape(arr) /= [dim1,dim1])) ERROR STOP 221
         if (rank(arr)      /=            2) ERROR STOP 222
         if (any(lbound(arr)    /=        [1,1])) ERROR STOP 223
         if (any(ubound(arr)    /=  [dim1,dim1])) ERROR STOP 224
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "array is C"
         if (size(arr)      /=    dim1*dim1*dim1) ERROR STOP 230
         if (any(shape(arr) /= [dim1,dim1,dim1])) ERROR STOP 231
         if (rank(arr)      /=            3) ERROR STOP 232
         if (any(lbound(arr)    /=        [1,1,1])) ERROR STOP 233
         if (any(ubound(arr)    /=  [dim1,dim1,dim1])) ERROR STOP 234
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "array is D"
         if (size(arr)      /=                  14400) ERROR STOP 240
         if (any(shape(arr) /= [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 241
         if (rank(arr)      /=                     10) ERROR STOP 242
         if (any(lbound(arr)    /=  [1,1,1,1,1,1,1,1,1,1])) ERROR STOP 243
         if (any(ubound(arr)    /=  [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 244
     else
         print*, "Error: this case should not exist"
         ERROR STOP 200
     endif
end subroutine sub
