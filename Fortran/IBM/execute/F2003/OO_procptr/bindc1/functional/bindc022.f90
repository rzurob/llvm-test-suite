!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 06/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Sequence Association, try DERIVED BIND(C) types
!*                                        Dummy argument being explicit shape and assume-size array
!*                                        And try binding name specified in BIND statement containing spaces
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   use ISO_C_BINDING

   type, bind(C) :: base
      integer(C_INT) :: i1
      real(C_FLOAT)  :: r1
   end type

   interface
      subroutine printarray ( b1, b2 )  bind(c, name='        C_printarray    ' )    !<- binding name shall be 'C_printarray' with the spaces removed
         import base
         type(base), intent(in) :: b1(4)
         type(base), intent(in)  :: b2(2,*)
      end subroutine
   end interface

   procedure(printarray), pointer, bind(C) :: pp1

end module

program bindc022
   use m

   type(base) :: b1(8)
   type(base), allocatable :: b2(:,:)

   b1 = (/ (base(100_C_INT+i,1000.0_C_FLOAT+i), i=1_C_INT,8_C_INT) /)

   allocate ( b2(2,4) )

   b2 = reshape ( source = (/ (base(200_C_INT+i,2000.0_C_FLOAT+i), i=1_C_INT,8_C_INT) /), shape = (/2,4/) )

   pp1 => printarray

   call pp1 ( b1(1), b2(1,3) )            !<- sequence associate with b1(1:4), b2(1:2,3:4)
   call pp1 ( b1(2:7), b2(1:2,3:2:-1) )   !<- sequence associate with b1(2:5), b2(1:2,3:2:-1)

   call pp1 ( b2(2,1), b1((/2,4,6,8/)) )  !<- sequence associate with b2(2:1:-1,2:3), b1((/2,4,6,8/)

end program
