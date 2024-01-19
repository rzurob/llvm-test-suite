!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              mixed implied-shape, assumed-size
!*                              assumed-len characters and assumed
!*                              type parameters
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   type base(l)
     integer, len :: l
     character :: c1
     character :: c2(l)
     integer :: i(l)
   end type

   character(*), parameter :: ishape1(*) = ['abcd','efgh']
   character(*), parameter :: ishape2(*,*) = reshape(['abcd','efgh'],[1,2])
   type(base(4)), parameter :: dtp1(*) = [ (base(4)('c','abcd',4),i=1,4) ]
   type(base(:)), allocatable :: dtp2(:)
   allocate (dtp2(4), source=dtp1)

   call sub1(ishape1,ishape2)
   call sub2(dtp1,dtp2)

   contains
   subroutine sub1(asize1,asize2)
     character(*) :: asize1(*)
     character(*) :: asize2(1,*)

     character(*), parameter :: ishape1a(*) = ['abcd','efgh']
     character(*), parameter :: ishape2a(*,*) = reshape(['abcd','efgh'],[1,2])

     do i=1,2
       if (asize1(i) .NE. ishape1a(i)) then
         error stop 1
       endif
       if (asize2(1,i) .NE. ishape2a(1,i)) then
         error stop 2
       endif
     end do
   end subroutine
   subroutine sub2(asizedtp1,asizedtp2)
     type(base(*)) :: asizedtp1(*)
     type(base(*)) :: asizedtp2(*)

     type(base(4)), parameter :: dtp1a(*) = [ (base(4)('c','abcd',4),i=1,4) ]
     type(base(:)), allocatable :: dtp2a(:)
     allocate (dtp2a(4), source=dtp1a)

     do i=1,2
       if (asizedtp1(i)%c1     .NE. dtp1a(i)%c1  .OR. &
         & ANY(asizedtp1(i)%c2 .NE. dtp1a(i)%c2) .OR. &
         & ANY(asizedtp1(i)%i  .NE. dtp1a(i)%i)) then
         error stop 3
       endif
       if (asizedtp2(i)%c1     .NE. dtp2a(i)%c1  .OR. &
         & ANY(asizedtp2(i)%c2 .NE. dtp2a(i)%c2) .OR. &
         & ANY(asizedtp2(i)%i  .NE. dtp2a(i)%i)) then
         error stop 4
       endif
     end do
   end subroutine
   end

