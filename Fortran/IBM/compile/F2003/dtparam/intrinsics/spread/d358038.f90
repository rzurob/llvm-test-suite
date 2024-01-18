!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358038.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 27 2008 
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
!*  DEFECT 358038
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len :: l1
  end type
  type,extends(base) :: child(l2)
     integer,len     :: l2
     class(base(l2)),pointer :: poly2(:)=>null()
  end type

end module
program d358038
  use m
  implicit none

  type(child(2,2)) :: child1=child2,2)() !<== wrong syntax

end program

