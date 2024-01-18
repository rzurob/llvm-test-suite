!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359002.f
!*
!*  DATE                       : Nov. 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Dummy Argument with deferred length
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  Defect 359002
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
  end type
type,extends(base) :: child(l2)
     integer,len   :: l2
end type
end module

program d359002

use m
implicit none
type(child(2,3,:)),pointer    :: child1

allocate(child(2,:,4) :: child1)

end
