!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359972.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 10 2008 
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
!*  defect 359972
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type outer(k2,l2)
     integer,kind :: k2
     integer,len  :: l2
     character(l2+1)  :: c(3:5)
     contains
        procedure,pass :: writeOuter
   end type
   contains
      subroutine writeOuter(this)
          class(outer(4,*)),intent(in) :: this
          select type(this)
             type is(outer(4,*))
              write(*,'(a5/a5/a5)') this    
              write(*,'(a5/a5/a5)') this%c 
             class default
                stop
          end select
      end subroutine
end module

program d359972

  use m
  implicit none

  type(outer(4,2)),target :: outer1
  outer1%c=["foo","red","pot"]
  call outer1%writeOuter

end program
