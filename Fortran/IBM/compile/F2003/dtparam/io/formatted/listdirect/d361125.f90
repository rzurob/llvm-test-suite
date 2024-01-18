!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 20 2009
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program d361125
  type DT1
     sequence
     real :: r(3)
  end type
  type(DT1) :: t1,t2
  equivalence(t1,t2)

end

