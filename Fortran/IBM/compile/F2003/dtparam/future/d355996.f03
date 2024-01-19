!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 12 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 355996
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer,len  :: l=3
  end type
end module

program d355996
    use m
    implicit none

    type(A) :: a1=A()()
    type(A) :: a2(2)=[A()(),A()()]

end program

