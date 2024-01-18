!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357266.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 8 2008 
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
!*  1. DEFECT 357266
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B
     procedure(),nopass,pointer :: procptr=>null()
  end type
  contains
     subroutine sub()
     end subroutine
end module

program d357266

  use m
  implicit none
  type(B),allocatable :: b1

  allocate(b1,source=B(-2))  !<== wrong

end program

