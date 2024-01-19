!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. DEFECT 356425
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer(2),len :: l1
      contains
         procedure :: get
   end type
   contains
      function get(dt)
          class(base(*)),intent(in) :: dt
          type(base(dt%l1))  :: get
          get=dt
      end function
end module

program d356425
   use m
   implicit none

   class(base(:)),allocatable :: b1
   allocate(b1,source=base(3)())

   call check(b1)
   contains
      subroutine check(dt)
          class(base(*)),intent(in) :: dt
          select type(dt)
              type is(base(*))
                print *,dt%get   !<=== wrong syntax
              class default
                error stop 10_4
          end select
      end subroutine

end program

