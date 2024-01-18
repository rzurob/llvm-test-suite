!*********************************************************************
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
!* DESCRIPTION                  : Fortran Bind(c) procedure called from Fortran
!*                                - Nesting of calls
!*                                   Bind(c) ==> Non-bind(c)
!*                                - contiguous attribute
!* Fortran array:
!*   - dim 1 is number of rows
!*   - dim2 is number of columns
!*   - Column order
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
      integer, parameter :: DIM_SIZE = 10, STRIDE = 2

      contains
      subroutine contig_sub(arr) bind(c)
        integer, contiguous :: arr(:,:) ! assumed-shape array in Bind(c) procedure not supported yet

        block
        integer i

            if ( lbound(arr,1) /=                 1 )  ERROR STOP 50
            if ( lbound(arr,2) /=                 1 )  ERROR STOP 51
            if ( ubound(arr,1) /=   DIM_SIZE/STRIDE )  ERROR STOP 52
            if ( ubound(arr,2) /=          DIM_SIZE )  ERROR STOP 53
            if ( size(arr)     /= (DIM_SIZE/STRIDE)*DIM_SIZE ) ERROR STOP 54
            if ( any(arr       /= reshape([((2*i-1), i=1, (DIM_SIZE/STRIDE)*DIM_SIZE)], [DIM_SIZE/STRIDE, DIM_SIZE])) ) ERROR STOP 55
        end block

        call sub(arr)
      end subroutine

      subroutine sub(arg)
        integer :: arg(DIM_SIZE/STRIDE, DIM_SIZE)

        block
            integer i
            if ( lbound(arg,1) /=                 1 )  ERROR STOP 60
            if ( lbound(arg,2) /=                 1 )  ERROR STOP 61
            if ( ubound(arg,1) /=   DIM_SIZE/STRIDE )  ERROR STOP 62
            if ( ubound(arg,2) /=          DIM_SIZE )  ERROR STOP 63
            if ( size(arg)     /= (DIM_SIZE/STRIDE)*DIM_SIZE ) ERROR STOP 64
            if ( any(arg       /= reshape([((2*i-1), i=1, (DIM_SIZE/STRIDE)*DIM_SIZE)], [DIM_SIZE/STRIDE, DIM_SIZE])) ) ERROR STOP 65
        end block

        arg(:,1) = arg(:,1) + 1
      end subroutine
end module mod

program AllocatableDummyArgument116f
      use iso_c_binding
      use mod
      implicit none
      integer :: i
      integer, pointer :: ptr(:,:)
      integer, allocatable, target :: tgt(:,:)

      allocate(tgt(DIM_SIZE, DIM_SIZE))
      tgt = reshape([(i, i=1, DIM_SIZE*DIM_SIZE)], [DIM_SIZE, DIM_SIZE])

      if (.not. allocated(tgt))  ERROR STOP 11
      if ( lbound(tgt,1) /=                 1 )  ERROR STOP 12
      if ( lbound(tgt,2) /=                 1 )  ERROR STOP 13
      if ( ubound(tgt,1) /=          DIM_SIZE )  ERROR STOP 14
      if ( ubound(tgt,2) /=          DIM_SIZE )  ERROR STOP 15
      if ( size(tgt)     /= DIM_SIZE*DIM_SIZE )  ERROR STOP 16
      if ( any(tgt       /= reshape([(i, i=1, DIM_SIZE*DIM_SIZE)], [DIM_SIZE, DIM_SIZE])) ) ERROR STOP 17

      ptr => tgt(1:DIM_SIZE:STRIDE, :)

      if (.not. associated(ptr)) ERROR STOP 21
      if ( lbound(ptr,1) /=                 1 )  ERROR STOP 22
      if ( lbound(ptr,2) /=                 1 )  ERROR STOP 23
      if ( ubound(ptr,1) /=   DIM_SIZE/STRIDE )  ERROR STOP 24
      if ( ubound(ptr,2) /=          DIM_SIZE )  ERROR STOP 25
      if ( size(ptr)     /= (DIM_SIZE/STRIDE)*DIM_SIZE ) ERROR STOP 26
      if ( any(ptr       /= reshape([((2*i-1), i=1, (DIM_SIZE/STRIDE)*DIM_SIZE)], [DIM_SIZE/STRIDE, DIM_SIZE])) ) ERROR STOP 27

      call contig_sub(ptr)

      if (.not. associated(ptr)) ERROR STOP 31
      if ( lbound(ptr,1) /=                 1 )  ERROR STOP 32
      if ( lbound(ptr,2) /=                 1 )  ERROR STOP 33
      if ( ubound(ptr,1) /=   DIM_SIZE/STRIDE )  ERROR STOP 34
      if ( ubound(ptr,2) /=          DIM_SIZE )  ERROR STOP 35
      if ( size(ptr)     /= (DIM_SIZE/STRIDE)*DIM_SIZE ) ERROR STOP 36
      if ( any(ptr(:,1)           /= [2,4,6,8,10]) ) ERROR STOP 37
      if ( any(ptr(:,2:DIM_SIZE)  /= reshape([((2*i-1)+DIM_SIZE, &
    &        i=1, DIM_SIZE/STRIDE*(DIM_SIZE-1))], [DIM_SIZE/STRIDE, DIM_SIZE-1])) ) ERROR STOP 38

      deallocate(tgt)

end program AllocatableDummyArgument116f
