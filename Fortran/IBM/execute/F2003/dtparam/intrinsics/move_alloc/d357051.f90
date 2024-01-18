!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357051.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 3 2008 
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
!*  1. DEFECT 357051
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l)
      integer,len :: l
      character(l),pointer  :: c2=>null() 
   end type
end module

program d357051
  use m
  implicit none
  type(dtp(:)),allocatable  :: from1

  allocate(dtp(5) :: from1)
  allocate(from1%c2,source="IBM")

  if(from1%l /= 5)                               error stop 10_4
  if(from1%c2 /= "IBM")                          error stop 11_4

end program

