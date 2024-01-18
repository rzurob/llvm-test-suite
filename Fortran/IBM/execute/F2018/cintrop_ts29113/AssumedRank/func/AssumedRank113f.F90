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
!*                                - implicit typing
!*                                - Call to BIND(C)/Non-BIND(C) procedure from different scopes:
!*                                main program, module, external and internal procedure
!*                                - Interface block appears in a module
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
module mod
   integer, parameter   :: dim1 = 5

   interface
     subroutine sub_bind_c(Iarg, test) bind(C)
         integer :: test
         allocatable Iarg(..)
     end subroutine sub_bind_c
     subroutine sub(Iarg, test)
         integer :: test
         allocatable Iarg(..)
     end subroutine sub
     subroutine sub_ext(Iarg, flag) bind(C)
         allocatable Iarg(..)
         integer flag
     end subroutine sub_ext
   end interface

   #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
   #else
      logical :: DEBUG_MODE = .FALSE.
   #endif

   contains
   subroutine sub_mod(Iarg, flag)
     allocatable Iarg(..)
     integer flag

     if (.not. allocated(Iarg)) ERROR STOP 50

!---------- call BIND(C) procedure from main program
     call sub_bind_c(Iarg, flag)

!---------- call non-BIND(C) procedure from main program
     call sub(Iarg, flag)
   end subroutine sub_mod
end module mod

program AssumedRank110f
   use mod
   integer :: i, j, k
   integer, allocatable :: sumv, A(:), B(:,:), C(:,:,:), D(:,:,:,:,:,:,:,:,:,:)


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
   call sub_int(sumv, 0)
   call sub_int(A, 1)
   call sub_int(B, 2)
   call sub_int(C, 3)
   call sub_int(D, 4)

!---------- external subprogram ----------
   call sub_ext(sumv, 0)
   call sub_ext(A, 1)
   call sub_ext(B, 2)
   call sub_ext(C, 3)
   call sub_ext(D, 4)

!---------- module subprogram ----------
   call sub_mod(sumv, 0)
   call sub_mod(A, 1)
   call sub_mod(B, 2)
   call sub_mod(C, 3)
   call sub_mod(D, 4)

contains
  subroutine sub_int(Iarg, flag)
     allocatable Iarg(..)
     integer flag

     if (.not. allocated(Iarg)) ERROR STOP 50

!---------- call BIND(C) procedure from main program
     call sub_bind_c(Iarg, flag)

!---------- call non-BIND(C) procedure from main program
     call sub(Iarg, flag)
  end subroutine sub_int
end program AssumedRank110f

subroutine sub_ext(Iarg, flag) bind(C)
     allocatable Iarg(..)
     integer flag

     interface
       subroutine sub_bind_c(Iarg, test) bind(C)
           integer :: test
           allocatable Iarg(..)
       end subroutine sub_bind_c
       subroutine sub(Iarg, test)
           integer :: test
           allocatable Iarg(..)
       end subroutine sub
     end interface

     if (.not. allocated(Iarg)) ERROR STOP 50

!---------- call BIND(C) procedure from main program
     call sub_bind_c(Iarg, flag)

!---------- call non-BIND(C) procedure from main program
     call sub(Iarg, flag)
end subroutine sub_ext

subroutine sub_bind_c(Iarg, test) bind(C)
     ! Dummy Iarg.
     integer :: test
     allocatable Iarg(..)
     ! Internal variables
     integer, parameter :: dim1 = 5

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (.not. allocated(Iarg))  ERROR STOP 101
     if (DEBUG_MODE) then
        print*, test
        print*, size(Iarg)
        print*, shape(Iarg)
        print*, rank(Iarg)
        print*, lbound(Iarg)
        print*, ubound(Iarg)
     endif

     if (test == 0) then
         if (DEBUG_MODE) print*, "scalar dummy Iarg."
         if (size(Iarg)      /=            1) ERROR STOP 110
         if (rank(Iarg)      /=            0) ERROR STOP 113
     elseif (test == 1) then
         if (DEBUG_MODE) print*, "Iargay is A"
         if (size(Iarg)      /=         dim1) ERROR STOP 113
         if (any(shape(Iarg) /=      [dim1])) ERROR STOP 113
         if (rank(Iarg)      /=            1) ERROR STOP 114
         if (any(lbound(Iarg)   /=      [1])) ERROR STOP 115
         if (any(ubound(Iarg)   /=   [dim1])) ERROR STOP 116
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "Iargay is B"
         if (size(Iarg)           /=    dim1*dim1) ERROR STOP 120
         if (any(shape(Iarg)      /= [dim1,dim1])) ERROR STOP 121
         if (rank(Iarg)           /=            2) ERROR STOP 122
         if (any(lbound(Iarg)    /=        [1,1])) ERROR STOP 123
         if (any(ubound(Iarg)    /=  [dim1,dim1])) ERROR STOP 124
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "Iargay is C"
         if (size(Iarg)          /=     dim1*dim1*dim1) ERROR STOP 130
         if (any(shape(Iarg)     /=  [dim1,dim1,dim1])) ERROR STOP 131
         if (rank(Iarg)          /=                  3) ERROR STOP 132
         if (any(lbound(Iarg)    /=           [1,1,1])) ERROR STOP 133
         if (any(ubound(Iarg)    /=  [dim1,dim1,dim1])) ERROR STOP 134
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "Iargay is D"
         if (size(Iarg)          /=                   14400) ERROR STOP 140
         if (any(shape(Iarg)     /=  [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 141
         if (rank(Iarg)          /=                      10) ERROR STOP 142
         if (any(lbound(Iarg)    /=  [1,1,1,1,1,1,1,1,1,1])) ERROR STOP 143
         if (any(ubound(Iarg)    /=  [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 144
     else
         print*, "Error: this case should not exist"
         ERROR STOP 100
     endif
end subroutine sub_bind_c

subroutine sub(Iarg, test)
     ! Dummy Iarg.
     integer :: test
     allocatable Iarg(..)
     ! Internal variables
     integer, parameter :: dim1 = 5

     #if defined (TC_DEBUG)
      logical :: DEBUG_MODE = .TRUE.
     #else
      logical :: DEBUG_MODE = .FALSE.
     #endif

     if (.not. allocated(Iarg))  ERROR STOP 201
     if (DEBUG_MODE) then
        print*, test
        print*, size(Iarg)
        print*, shape(Iarg)
        print*, rank(Iarg)
        print*, lbound(Iarg)
        print*, ubound(Iarg)
     endif

     if (test == 0) then
         if (DEBUG_MODE) print*, "scalar dummy Iarg."
         if (size(Iarg)      /=            1) ERROR STOP 210
         if (rank(Iarg)      /=            0) ERROR STOP 211
     elseif (test == 1) then
         if (DEBUG_MODE) print*, "Iargay is A"
         if (size(Iarg)      /=         dim1) ERROR STOP 212
         if (any(shape(Iarg) /=      [dim1])) ERROR STOP 213
         if (rank(Iarg)      /=            1) ERROR STOP 214
         if (any(lbound(Iarg)   /=      [1])) ERROR STOP 215
         if (any(ubound(Iarg)   /=   [dim1])) ERROR STOP 216
     elseif (test == 2) then
         if (DEBUG_MODE) print*, "Iargay is B"
         if (size(Iarg)      /=       dim1*dim1) ERROR STOP 220
         if (any(shape(Iarg) /=    [dim1,dim1])) ERROR STOP 221
         if (rank(Iarg)      /=               2) ERROR STOP 222
         if (any(lbound(Iarg)   /=       [1,1])) ERROR STOP 223
         if (any(ubound(Iarg)   /= [dim1,dim1])) ERROR STOP 224
     elseif (test == 3) then
         if (DEBUG_MODE) print*, "Iargay is C"
         if (size(Iarg)      /=        dim1*dim1*dim1) ERROR STOP 230
         if (any(shape(Iarg) /=     [dim1,dim1,dim1])) ERROR STOP 231
         if (rank(Iarg)      /=                     3) ERROR STOP 232
         if (any(lbound(Iarg)    /=          [1,1,1])) ERROR STOP 233
         if (any(ubound(Iarg)    /= [dim1,dim1,dim1])) ERROR STOP 234
     elseif (test == 4) then
         if (DEBUG_MODE) print*, "Iargay is D"
         if (size(Iarg)      /=                    14400) ERROR STOP 240
         if (any(shape(Iarg) /=   [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 241
         if (rank(Iarg)      /=                       10) ERROR STOP 242
         if (any(lbound(Iarg) /=  [1,1,1,1,1,1,1,1,1,1])) ERROR STOP 243
         if (any(ubound(Iarg) /=  [1,2,3,4,5,1,2,3,4,5])) ERROR STOP 244
     else
         print*, "Error: this case should not exist"
         ERROR STOP 200
     endif
end subroutine sub
