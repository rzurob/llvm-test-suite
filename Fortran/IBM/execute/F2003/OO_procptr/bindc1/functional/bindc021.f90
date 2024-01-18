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
!*                                        Sequence Association, try BIND(C) INTRINSIC types (INTEGER, REAL)
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

   interface
      subroutine ESarray ( i1, r1 )  bind(c, name='        C_ESarray    ' )    !<- binding name shall be 'C_ESarray' with the spaces removed
         import C_INT, C_FLOAT
         integer(C_INT), intent(in) :: i1(4)
         real(C_FLOAT), intent(in)  :: r1(4)
      end subroutine
   end interface

   interface
      subroutine ASarray ( i1, r1 )  bind(c, name='        C_ASarray    ' )    !<- binding name shall be 'C_ESarray' with the spaces removed
         import C_INT, C_FLOAT
         integer(C_INT), intent(in) :: i1(*)
         real(C_FLOAT), intent(in)  :: r1(2:3,*)
      end subroutine
   end interface

   procedure(ESarray), pointer, bind(C) :: pp1

end module

program bindc021
   use m

   procedure(ASarray), pointer, bind(C) :: pp2

   integer(C_INT) :: i11(8)
   real(C_FLOAT)  :: r11(8)

   i11 = (/ ( i, i=101_C_INT,108_C_INT ) /)
   r11 = (/ ( s, s=201.0_C_FLOAT,208.0_C_FLOAT ) /)

   pp1 => ESarray
   pp2 => ASarray

   call pp1 ( i11(2), r11(5) )             !<- sequence associate with i11(2:5), r11(5:8)
   call pp1 ( i11, r11((/1,1,2,4,5,6/)) )  !<- sequence associate with i11(1:4), r11(1,2,2,4)

   call pp2 ( i11(5), r11(3) )             !<- sequence associate with i11(5:8), r11(3:8)
   call pp2 ( i11(4:1:-1), r11((/1,1,2,2,3,3/)) )             !<- sequence associate with i11(4:1:-1), r11((/1,1,2,2,3,3/))

end program

