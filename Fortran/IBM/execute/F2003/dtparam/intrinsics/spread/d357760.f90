!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357760.f   
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
!*  1. DEFECT 357760
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l)
     integer,len     :: l
     character(l)    :: c(l-1) 
   end type
end module
  use m
  implicit none

  type(dtp(3)) :: dtp1=dtp(3)(c=["1","2"])
  call check()   
  write (*,'(4a2)') getresult()
  contains
     subroutine check()
          type(dtp(:)),allocatable :: tmp(:)
          tmp=spread(dtp1,1,2)
          write (*,'(4a2)') tmp
     end subroutine
     function getresult()
          type(dtp(:)),allocatable :: getresult(:)
          getresult=spread(dtp1,1,2)
     end function
  end
