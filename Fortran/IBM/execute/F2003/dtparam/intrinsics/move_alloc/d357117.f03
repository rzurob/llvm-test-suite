!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357117
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
     integer,kind :: k
     integer,len  :: l
     integer(k)   :: i(l)
     contains
        final  :: finaldtp
  end type

  contains
     subroutine finaldtp(dt)
         type(dtp(2,*)),intent(in) :: dt
         print *,"finalize :",dt%i
     end subroutine
end module

program d357117
  use m
  implicit none

  type(dtp(2,4)),allocatable :: from2

  from2=dtp(2,4)(i=[-1,-2,-3,-4])

end program
