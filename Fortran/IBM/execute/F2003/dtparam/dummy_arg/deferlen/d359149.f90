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
!*  DEFECT 359149
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
       integer,len   :: l=3
       character(l)  :: c="xlf"
   end type
   contains
       subroutine sub3(arg)
          type(base(*)) :: arg
          arg=base(arg%l)()
       end subroutine
end module
program d359149
  use m
  implicit none

  type(base(3))  :: b=base(3)()

  call sub3(b)

end program
