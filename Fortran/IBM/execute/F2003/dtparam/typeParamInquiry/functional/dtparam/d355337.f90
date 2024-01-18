!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355337.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 24 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
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
!* 1. TEST SECTION 6.1.3 
!* 2. DEFECT 355337
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type B(lb)
      integer,len :: lb
      character(lb) :: c1
      character(:),allocatable :: c2
   end type
   type A(la)
      integer,len   :: la
      type(B(2*la)) :: b2  
   end type
end module

program d355337
  use m
  implicit none

  type(A(3)) :: a1
  type(A(:)),allocatable :: a2

  a1%b2%c1="fortran"
  a1%b2%c2="xlftest team"

  a2=a1

  print *,"before assignment"

  print *,a1%b2%lb
  print *,a1%b2%c1%len,len(a1%b2%c1)
  print *,a1%b2%c1
  print *,a1%b2%c2%len,len(a1%b2%c2)
  print *,a1%b2%c2

  print *,"after assignment"
  print *,a2%b2%lb
  print *,a2%b2%c1%len,len(a2%b2%c1)
  print *,a2%b2%c1
  print *,a2%b2%c2%len,len(a2%b2%c2)
  print *,a2%b2%c2

end
