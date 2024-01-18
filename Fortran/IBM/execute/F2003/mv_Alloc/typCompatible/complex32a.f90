! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of unlimited poly, component of a DT
!*                               TO/FROM of complex*32 with many attrs
!*                               be called in module procedure
!*                               rank 5
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

       complex*32, allocatable, save, protected, volatile :: a1(:,:,:,:,:)

       contains
          subroutine sub(a2)
              complex(16), intent(inout) :: a2(:,:,:,:,:)
              allocatable a2
              call move_alloc(a1, a2)
          end subroutine

          subroutine new
              allocate(a1(1,1,1,1,1))
              a1 = cmplx(cmplx(1,2,16))
          end subroutine
end module

program main

       use m
       complex(16), allocatable :: x(:,:,:,:,:)

       call new

       call sub(x)

       if ( .not. allocated(x) ) stop 21
       if ( allocated(a1) ) stop 23

       write (6, 100) x(1,1,1,1,1)

       100 format ("(", 2f25.17, ")")
end
