!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 25 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY
!* 3. DEFECT 354300
!234567890123456789012345678901234567890123456789012345678901234567890
module m

    type base(k1,l1)
          integer,kind :: k1
          integer(k1%kind),len :: l1
    end type

    type,extends(base) :: child(k2,l2)
          integer(kind(k1)),kind :: k2
          integer,len :: l2
    end type

    contains
    subroutine check1(dt)
         class(base(2,:)),allocatable,intent(in) :: dt

         print *,"call check1"
         select type(dt)
           type is(base(2,*))
             print *,"dt is base"
           type is(child(2,*,4,*))
             print *,"dt is child"
           class default
             print *,"should not come here"
         end select
    end subroutine

end module

  program dtParameterInquirySelectTypeParam04_d354300
  use m
  implicit none


end

