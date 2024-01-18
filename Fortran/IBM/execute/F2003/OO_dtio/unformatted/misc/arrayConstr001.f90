!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: array constructor 
!*                                        problem found in defect 298826
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program arrayConstr001

   type :: mydata
      integer(4) ::  i
   end type

   type :: base
      type(mydata) :: b(2)
      character(3) :: c
   end type

   type(base) :: b1
   type(mydata), allocatable :: d1(:)

   allocate ( d1(2), source = (/ mydata(1), mydata(2) /) )
   b1 = base( b = d1, c = 'ibm' )

   if ( ( b1%b(1)%i /= 1 )  .or. ( b1%b(2)%i /= 2 ) .or. ( b1%c /= 'ibm' ) ) error stop 1_4
end
