!*********************************************************************
!*  ===================================================================
!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d354657.f
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
   contains
      character function getchar()
         getchar='A'
      end function
end module

program d354657
    use m
    implicit none
    integer i

     associate(x=>getchar(i))
     end associate
end
