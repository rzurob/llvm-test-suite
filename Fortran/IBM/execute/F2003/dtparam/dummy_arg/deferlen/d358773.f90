!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358773.f
!*
!*  DATE                       : Nov. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 358773
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1)
     integer,len     :: l1
     character(l1)   :: c1
  end type
  contains
   function fun1(arg1)
       type(dtp(*)),intent(in)   :: arg1(2)
       type(dtp(3))              :: fun1(2)

       fun1=arg1
   end function
end module

program d358773
  use m
  implicit none

  type(dtp(:)),allocatable :: dtp1(:)

  allocate(dtp1(2),source=[dtp(3)("xlf"),dtp(3)("xlc")])
  print *,fun1(dtp1)

end program
