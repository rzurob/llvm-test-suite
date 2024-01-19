!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 6 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. USE ASSOCIATE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(len=3)         :: c1(3)="abc"
   contains
      function getchar(c1,c2)
         character(*),intent(in) :: c1(:),c2(:)
         character(c1%len+c2%len):: getchar(size(c1)+size(c2))
         getchar=c1(1)//c2(1)
      end function
end module

program typeParamInquiryIntrinsicAssociate02_d354675

    use m
    implicit none
    integer :: k
      associate(x=>getchar(c1,c1))
        do k=1,size(x)
          print *,x(k)
        enddo
     end associate
end

