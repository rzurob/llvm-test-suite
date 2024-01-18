! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorPass007d.f
! opt variations: -ql

!+  ===================================================================
!+  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!+  ===================================================================
!+  ===================================================================
!+
!+  TEST CASE TITLE            :
!+
!+  PROGRAMMER                 : Robert Ma
!+  DATE                       : 11/01/2005
!+  ORIGIN                     : AIX Compiler Development, Toronto Lab
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DRIVER STANZA              : xlf95
!+
!+  DESCRIPTION                : Binary Operator: with pass attribute with another derived type (ambiguous)
!+  KEYWORD(S)                 :
!+  TARGET(S)                  :
!+ ===================================================================
!+
!+  REVISION HISTORY
!+
!+  MM/DD/YY:  Init:  Comments:
!+ ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: int_base1_base
         generic :: operator(+) => int_base1_base
   end type
   
   type base1(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: j = -999
      contains
         procedure, pass :: int_base1_base_amb
         generic :: operator(+) => int_base1_base_amb
   end type

   contains

   integer function int_base1_base ( a, b )
      class(base1(4)), intent(in) :: a
      class(base(4)), intent(in) :: b
      int_base1_base=10
   end function
   
   integer function int_base1_base_amb ( a, b )
      class(base1(4)), intent(in) :: a
      class(base(4)), intent(in) :: b
      int_base1_base_amb=20
   end function

end module

program genericOperatorPass007d
end program
