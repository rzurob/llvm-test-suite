!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358772.f   
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
!*  DEFECT 358772
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1)
     integer,len     :: l1
     character(l1)   :: c1
  end type
  contains
   elemental function fun1(arg1)
       type(dtp(3)),intent(in)      :: arg1 
       type(dtp(3))      :: fun1 
       fun1=arg1
   end function
end module

program d358772
use m
implicit none

  type(dtp(:)),allocatable :: dtp1(:)

  allocate(dtp1(2),source=[dtp(3)("xlf"),dtp(3)("xlc")])
  print *,fun1(dtp1)

end program
