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
!*                                - dot_product
!*                                - Call to BIND(C) procedure from different scopes:
!*                                main program, module and internal procedure
!*                                - Nesting: call chain:
!*                                non-Bind(C) => Bind(C) => non-Bind(C)
!*                                module/internal/main => external => external
!*                                - Formatted I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
   use iso_c_binding, only: c_int
   implicit none
   integer, parameter  :: lower = 1, upper = 12

   interface
     subroutine sub_bind_c(a,b,c,d,e,f,g) bind(C)
       use iso_c_binding, only: c_int
       implicit none
       integer(c_int) :: i, j, k
       integer(c_int), allocatable :: a(:,:)
       integer(c_int), allocatable :: b(:,:)
       integer(c_int), allocatable :: c(:,:)
       integer(c_int), allocatable :: d(:,:)
       integer(c_int), allocatable :: e(:,:)
       integer(c_int), allocatable :: f(:,:)
       integer(c_int), allocatable :: g(:,:)
     end subroutine sub_bind_c
   end interface

   contains
   subroutine sub_mod(a,b,c,d,e,f,g)
     integer(c_int), allocatable :: a(:,:)
     integer(c_int), allocatable :: b(:,:)
     integer(c_int), allocatable :: c(:,:)
     integer(c_int), allocatable :: d(:,:)
     integer(c_int), allocatable :: e(:,:)
     integer(c_int), allocatable :: f(:,:)
     integer(c_int), allocatable :: g(:,:)

     if (.not. allocated(a)) error stop 22
     if (.not. allocated(b)) error stop 23
     if (.not. allocated(c)) error stop 24
     if (.not. allocated(d)) error stop 25
     if (.not. allocated(e)) error stop 26
     if (.not. allocated(f)) error stop 27
     if (.not. allocated(g)) error stop 28

!---------- call BIND(C) procedure from Module procedure
     call sub_bind_c(a,b,c,d,e,f,g)

   end subroutine sub_mod
end module mod

program AllocatableDummyArgument108f
   use iso_c_binding, only: c_int
   use mod
   implicit none

   integer(c_int), allocatable :: a(:,:)
   integer(c_int), allocatable :: b(:,:)
   integer(c_int), allocatable :: c(:,:)
   integer(c_int), allocatable :: d(:,:)
   integer(c_int), allocatable :: e(:,:)
   integer(c_int), allocatable :: f(:,:)
   integer(c_int), allocatable :: g(:,:)
   integer lb1, ub1, lb2, ub2, num
   integer i, j

   lb1 = lower
   lb2 = lower
   ub1 = upper
   ub2 = upper

   allocate(a(upper,upper),b(upper,upper),c(upper,upper),d(upper,upper))
   allocate(e(upper,upper),f(upper,upper),g(upper,upper))

   if (.not. allocated(a)) error stop 2
   if (.not. allocated(b)) error stop 3
   if (.not. allocated(c)) error stop 4
   if (.not. allocated(d)) error stop 5
   if (.not. allocated(e)) error stop 6
   if (.not. allocated(f)) error stop 7
   if (.not. allocated(g)) error stop 8

   num = 1
   do i = lb1, ub1
    do j = lb2, ub2
     if (num .gt. 5 ) then
         num = 1
     endif
     a(i,j) = num
     b(i,j) = 5  - num + 1
     num    = num + 1
    enddo
   enddo
   c(:,:) = 0
   d(:,:) = a(:,:)
   e(:,:) = a(:,:)
   f(:,:) = a(:,:)
   g(:,:) = b(:,:)

   open(10, file='output.dat')
!---------- call BIND(C) procedure from main program for dot_product computation
   num = 1
   do i = lb1, ub1
    do j = lb2, ub2
     if (num .gt. 5 ) then
         num = 1
     endif
     a(i,j) = num
     b(i,j) = 5  - num + 1
     num    = num + 1
    enddo
   enddo
   c(:,:) = 0
   d(:,:) = a(:,:)
   e(:,:) = a(:,:)
   f(:,:) = a(:,:)
   g(:,:) = b(:,:)

   call sub_bind_c(a,b,c,d,e,f,g)

!---------- internal subprogram ----------
   num = 1
   do i = lb1, ub1
    do j = lb2, ub2
     if (num .gt. 5 ) then
         num = 1
     endif
     a(i,j) = num
     b(i,j) = 5  - num + 1
     num    = num + 1
    enddo
   enddo
   c(:,:) = 0
   d(:,:) = a(:,:)
   e(:,:) = a(:,:)
   f(:,:) = a(:,:)
   g(:,:) = b(:,:)

   call sub_int(a,b,c,d,e,f,g)

!---------- module subprogram ----------
   num = 1
   do i = lb1, ub1
    do j = lb2, ub2
     if (num .gt. 5 ) then
         num = 1
     endif
     a(i,j) = num
     b(i,j) = 5  - num + 1
     num    = num + 1
    enddo
   enddo
   c(:,:) = 0
   d(:,:) = a(:,:)
   e(:,:) = a(:,:)
   f(:,:) = a(:,:)
   g(:,:) = b(:,:)

   call sub_mod(a,b,c,d,e,f,g)

   close(10)
contains
  subroutine sub_int(a,b,c,d,e,f,g)
     integer(c_int), allocatable :: a(:,:)
     integer(c_int), allocatable :: b(:,:)
     integer(c_int), allocatable :: c(:,:)
     integer(c_int), allocatable :: d(:,:)
     integer(c_int), allocatable :: e(:,:)
     integer(c_int), allocatable :: f(:,:)
     integer(c_int), allocatable :: g(:,:)

     if (.not. allocated(a)) error stop 12
     if (.not. allocated(b)) error stop 13
     if (.not. allocated(c)) error stop 14
     if (.not. allocated(d)) error stop 15
     if (.not. allocated(e)) error stop 16
     if (.not. allocated(f)) error stop 17
     if (.not. allocated(g)) error stop 18

!---------- call BIND(C) procedure from internal procedure
     call sub_bind_c(a,b,c,d,e,f,g)

  end subroutine sub_int
end program AllocatableDummyArgument108f

subroutine sub_bind_c(a,b,c,d,e,f,g) bind(C)
     use iso_c_binding, only: c_int
     implicit none
     integer(c_int) :: i, j, k
     integer(c_int), allocatable :: a(:,:)
     integer(c_int), allocatable :: b(:,:)
     integer(c_int), allocatable :: c(:,:)
     integer(c_int), allocatable :: d(:,:)
     integer(c_int), allocatable :: e(:,:)
     integer(c_int), allocatable :: f(:,:)
     integer(c_int), allocatable :: g(:,:)

     interface
        subroutine sub_io(id,a,b,c,d) bind(C)
           use iso_c_binding, only: c_int
           implicit none
           integer(c_int) :: id
           integer(c_int), allocatable :: a(:,:), b(:,:), c(:,:), d(:,:)
        end subroutine sub_io
     end interface

     if (.not. allocated(a)) error stop 2
     if (.not. allocated(b)) error stop 3
     if (.not. allocated(c)) error stop 4
     if (.not. allocated(d)) error stop 5
     if (.not. allocated(e)) error stop 6
     if (.not. allocated(f)) error stop 7
     if (.not. allocated(g)) error stop 8

     do i = 1, 12, 4
      do j = 1, 12, 4
       c(i  ,j  ) = dot_product(a(:,i), b(:,j))
       c(i+1,j+1) = dot_product(a(:,j), b(i,:))
       c(i+2,j+2) = dot_product(a(i,i:j), b(j,i:j))
       c(i+3,j+3) = dot_product(a(:,j), a(i,:))
      enddo
     enddo

     do i = 1+3, 12-3, 3
      do j = 1+3, 12-3, 3
       d(i,j) = dot_product(d(:,j+3), b(:  ,i))
       e(i,j) = dot_product(e(:,j-3), e(:  ,i))
       f(i,j) = dot_product(f(:,j+3), f(i-3,:))
      enddo
     enddo

     call sub_io(10,c,d,e,f)
end subroutine sub_bind_c

subroutine sub_io(id,a,b,c,d) bind(C)
     use iso_c_binding, only: c_int
     implicit none
     integer(c_int) :: id
     integer(c_int) :: i, j, n, m, p, q
     integer(c_int), allocatable :: a(:,:), b(:,:), c(:,:), d(:,:)

     n = lbound(a,1)
     m = ubound(a,1)
     p = lbound(a,2)
     q = ubound(a,2)

     write(id,*) "*****************Test Bind(C) IO **************************"
     do j = n, m
      do i = p, q
       write(id,'(6i12)')i,j,a(i,j),b(i,j),c(i,j),d(i,j)
      enddo
     enddo

end subroutine sub_io
