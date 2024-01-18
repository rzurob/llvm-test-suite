! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorPure001.f
! opt variations: -ql

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Binary Operator: UD operator subroutine is a pure function
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x = -999
      contains
         procedure, pass :: mybadd
         generic :: operator(*) => mybadd
   end type

   contains

   type(base(4)) pure function mybadd ( a, b )
      class(base(4)), intent(in) :: a
      integer, intent(in) :: b

      mybadd%x = a%x * b

   end function

end module

program genericOperatorPure001
   use m

   type(base(4)) :: b1 = base(4)(100)
   type(base(4)) :: b2 = base(4)(200)
   type(base(4)) :: b3
   
   b3 = b1 * 10
   print *, b3
   
   b2 = b3 * 5
   print *, b2
   
   b1 = base(4)(20) * 7
   print *, b1

end program
