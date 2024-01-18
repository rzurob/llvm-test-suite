!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359800.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 5 2008 
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
!*  defect 359800
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
     integer,len :: l1
     character(l1) :: c1(l1:l1)
  end type
end module

program d359800
  use m
  implicit none

  type(base(:)),allocatable :: tbase2 

  tbase2=base(3)(c1=["boo"])
  print *, tbase2
    
end program
