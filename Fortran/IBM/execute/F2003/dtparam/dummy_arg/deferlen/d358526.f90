!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358526.f   
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
!*  DEFECT 358526
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1,l2)
    integer,len  :: l1=3
    integer,len  :: l2=4
  end type
end module

program d358526
  use m
  implicit none

  contains

  subroutine sub1(arg)
    type(dtp(*,:)),pointer :: arg
  end subroutine

end program
