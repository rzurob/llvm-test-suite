!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359311.f
!*
!*  DATE                       : Nov. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359311
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1)
      integer,len   :: l1=3
      character(l1) :: c="xlf"
   end type
end module

program d359311
  use m
  implicit none

  type(dtp(3)) :: dt
  contains

    subroutine sub(dt)
      type(dtp(*)),intent(in) :: dt
        print *,dt%c//dt%c
    end subroutine
end
