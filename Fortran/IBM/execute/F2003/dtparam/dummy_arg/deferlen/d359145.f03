!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359145
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
       integer,len   :: l=3
       character(l) :: c="xlf"
   end type
   contains
       subroutine sub3(arg)
          type(base(3)) :: arg(:)
          integer :: i
          arg=(/ (base(arg%l)(), i=1,3) /)
       end subroutine

end module

program d359145
  use m
  implicit none

  type(base(3))  :: b(3)=base(3)()
  call sub3(b)

end program