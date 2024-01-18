!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: reshape intrinsic with polymorphic components (defect 299894)
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

   type :: mydata
      integer(4) ::  i1 = 1
   end type

   type, extends(mydata) :: mysuperdata
      integer(4) ::  i2 = 2
   end type

   type :: base
      class(mydata), pointer :: b
   end type

end module

use m

   type(base), pointer      :: b2(:)
   type(base)               :: b3(2,2)

   allocate( b2(2))
   allocate ( mysuperdata :: b2(1)%b, b2(2)%b &
              , b3(1,1)%b, b3(2,1)%b, b3(1,2)%b, b3(2,2)%b )

   associate ( g => b2(1)%b )
      select type (g)
         type is (mysuperdata)
            g%i1 = 105
            g%i2 = 106
      end select
   end associate

   associate ( g => b2(2)%b )
      select type (g)
         type is (mysuperdata)
            g%i1 = 107
            g%i2 = 108
      end select
   end associate

   b3 = reshape ( source = (/ b2, b2 /) , shape = (/2,2/) )

   associate ( g => b3(1,1)%b )
      select type (g)
         type is (mysuperdata)
            if ( ( g%i1 /= 105 ) .or. ( g%i2 /= 106 ) ) error stop 1_4
      end select
   end associate

   associate ( g => b3(2,1)%b )
      select type (g)
         type is (mysuperdata)
            if ( ( g%i1 /= 107 ) .or. ( g%i2 /= 108 ) ) error stop 2_4
      end select
   end associate

   associate ( g => b3(1,2)%b )
      select type (g)
         type is (mysuperdata)
            if ( ( g%i1 /= 105 ) .or. ( g%i2 /= 106 ) ) error stop 3_4
      end select
   end associate

   associate ( g => b3(2,2)%b )
      select type (g)
         type is (mysuperdata)
            if ( ( g%i1 /= 107 ) .or. ( g%i2 /= 108 ) ) error stop 4_4
      end select
   end associate

end

