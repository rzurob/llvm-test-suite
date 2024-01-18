!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 18 2008
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
!* 3. USE ASSOCIATE
!* 4. ASSOCIATE TO EXPRESSION
!* 5. DEFECT 354602 354013
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1,l2)
     integer(2),len :: l1
     integer(8),len :: l2
     character(l1+l2) :: c1(l1:l2)
  end type
end module

program dtParameterInquiryAssociate03
  use m
  implicit none

  type(base(:,:)),pointer :: b1
  character(:),pointer    :: c

  allocate(b1,source=base(2,4)(c1=['abcd','efgh','ijkl']))

  associate(  x=>b1%l1   + b1%l2+ &
              b1%l1%kind + kind(b1%l1) + &
              b1%l2%kind + kind(b1%l2) + &
              b1%c1%len  + len(b1%c1)  + &   ! defect 354013
              ubound(b1%c1,1)+lbound(b1%c1,1) )
    if(x /= 44)                                           error stop 10_4
  end associate

  associate(x=>b1%c1(3)(1:1)) ! defect 354602
    if(x%len /= len(x) .or. x%len /= 1)                   error stop 11_4
  end associate

  associate(x=>b1%c1(2)(1:2)) ! defect 354602
    if(x%len /= len(x) .or. x%len /= 2)                   error stop 12_4
  end associate

  associate(x=>b1%c1(3)(1:2)//'12') ! defect 354602
    if(x%len /= len(x) .or. x%len /= 4)                   error stop 13_4
  end associate

   associate(x=>getlen(b1)+2*(b1%l1+b1%l2))
      if(x /= 24)                                         error stop 14_4
      allocate(character(x/4) :: c)
      c="xlftest"
   end associate

   if(c%len /= 6)                                         error stop 15_4
   if(c /= "xlftes")                                      error stop 16_4

  contains

    integer function getlen(b)
        type(base(*,*)),intent(in) :: b
        associate(x=>b%l1+b%l2)
           getlen=2*x
        end associate
    end function
end
