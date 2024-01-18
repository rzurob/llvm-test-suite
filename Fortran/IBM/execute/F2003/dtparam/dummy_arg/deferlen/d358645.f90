!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358645.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 10 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  DEFECT 358645
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len  :: l1
    integer      :: i
  end type
end module

program d358645

use m
implicit none

type(base(:)),allocatable :: base1(:)
type(base(4))             :: base2=base(4)(1)
type(base(:)),allocatable :: base3

base1=[base(3)(3),base(3)(4),base(3)(5)]
base3=base2
print *,base3%l1, base3%i
base3=base1(1)   
print *,base3%l1,base3%i

end program
