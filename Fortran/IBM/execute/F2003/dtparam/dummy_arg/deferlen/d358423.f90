!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358423.f   
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
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1)
     integer,len :: l1
  end type
end module

program d358423

use m
implicit none

type(dtp(3)),pointer :: a1=>null()
allocate(dtp(3) :: a1)
call sub(a1)
contains
  subroutine sub(arg)
    type(dtp(*)),pointer :: arg
    print *,arg%l1
    allocate(dtp(*) :: arg)
  end subroutine

end program

