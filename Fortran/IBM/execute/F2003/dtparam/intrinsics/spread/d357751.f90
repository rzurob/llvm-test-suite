!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357751.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 21 2008 
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
!*  1. DEFECT 357751
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp
      integer :: i
   end type
end module

program d357751
  use m
  implicit none

  type(dtp) :: dtp1=dtp(i=1)
  write (*,'(2i2)') getSpreadResult1(1)
  contains
     function getSpreadResult1(dim)
          integer,intent(in) :: dim
          type(dtp),allocatable :: getSpreadResult1(:)

          getSpreadResult1=spread(dtp1,dim,2) 
     end function
end program

