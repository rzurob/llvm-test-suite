! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument114f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - pointer with 15 dimensions
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program AllocatableDummyArgument114f
      implicit none
      integer :: i
      integer(1), pointer :: ptr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
      integer(1), allocatable, target :: t(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
      integer, parameter :: N = 3

      interface
        subroutine sub_c(arr) bind(c)
          integer(1), pointer :: arr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        end subroutine
      end interface 

      allocate(t(N,N,N,N,N,N,N,N,N,N,N,N,N,N,N))

      ptr => t(1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,:)

      call sub_c(ptr)

      do i=1, 3, 1
        if (any(ptr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,i) .ne. i)) error stop 100
        if (any(t(1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,1:N:2,i) .ne. i)) error stop 101
      end do
      
      deallocate(t)
end program AllocatableDummyArgument114f

subroutine sub_c(arr) bind(c)
   integer(1), pointer :: arr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
   
   interface
     subroutine sub(arg) bind(c)
       integer(1) :: arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,3)
     end 
   end interface 

   do i=1, 14
     if(lbound(arr,i) .ne. 1) error stop 10
     if(ubound(arr,i) .ne. 2) error stop 11
   end do 
   if(lbound(arr,15) .ne. 1) error stop 12
   if(ubound(arr,15) .ne. 3) error stop 13

   call sub(arr)

   do i=1, 3, 1
     if (any(arr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,i) .ne. i) ) error stop 14 
   end do
end subroutine

subroutine sub(arg) bind(c)
   integer(1) :: arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,3)

   do i=1, 3, 1
     arg(:,:,:,:,:,:,:,:,:,:,:,:,:,:,i) = i
   end do

   do i=1, 14
     if(lbound(arg,i) .ne. 1) error stop 20
     if(ubound(arg,i) .ne. 2) error stop 21
   end do 
   if(lbound(arg,15) .ne. 1) error stop 22
   if(ubound(arg,15) .ne. 3) error stop 23

end subroutine
