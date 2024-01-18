!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : misc008kl
!*
!*  PROGRAMMER                 : David Forster (derived from misc008 by Robert Ma)
!*  DATE                       : 2007-07-06 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
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

   type :: mydata (kmd)
      integer, kind :: kmd
      integer(kmd) ::  i1 = 1
   end type

   type, extends(mydata) :: mysuperdata (kmsd)
      integer, kind :: kmsd
      integer(kmsd) ::  i2 = 2
   end type

   type :: base (kb)
      integer, kind :: kb
      class(mydata(kb)), pointer :: b ! tcx: (kb)
   end type

end module

use m

   type(base(4)), pointer      :: b2(:) ! tcx: (4)
   type(base(4))               :: b3(2,2) ! tcx: (4)

   allocate( b2(2))
   allocate ( mysuperdata(4,4) :: b2(1)%b, b2(2)%b & ! tcx: (4,4)
              , b3(1,1)%b, b3(2,1)%b, b3(1,2)%b, b3(2,2)%b )

   associate ( g => b2(1)%b )
      select type (g)
         type is (mysuperdata(4,4)) ! tcx: (4,4)
            g%i1 = 105
            g%i2 = 106
      end select
   end associate

   associate ( g => b2(2)%b )
      select type (g)
         type is (mysuperdata(4,4)) ! tcx: (4,4)
            g%i1 = 107
            g%i2 = 108
      end select
   end associate

   b3 = reshape ( source = (/ b2, b2 /) , shape = (/2,2/) )

   associate ( g => b3(1,1)%b )
      select type (g)
         type is (mysuperdata(4,4)) ! tcx: (4,4)
            if ( ( g%i1 /= 105 ) .or. ( g%i2 /= 106 ) ) error stop 1_4
      end select
   end associate

   associate ( g => b3(2,1)%b )
      select type (g)
         type is (mysuperdata(4,4)) ! tcx: (4,4)
            if ( ( g%i1 /= 107 ) .or. ( g%i2 /= 108 ) ) error stop 2_4
      end select
   end associate

   associate ( g => b3(1,2)%b )
      select type (g)
         type is (mysuperdata(4,4)) ! tcx: (4,4)
            if ( ( g%i1 /= 105 ) .or. ( g%i2 /= 106 ) ) error stop 3_4
      end select
   end associate

   associate ( g => b3(2,2)%b )
      select type (g)
         type is (mysuperdata(4,4)) ! tcx: (4,4)
            if ( ( g%i1 /= 107 ) .or. ( g%i2 /= 108 ) ) error stop 4_4
      end select
   end associate

end



! Extensions to introduce derived type parameters:
! type: mydata - added parameters (kmd) to invoke with (4) / declare with (4) - 1 changes
! type: mysuperdata - added parameters (kmsd) to invoke with (4,4) / declare with (4,4) - 7 changes
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 2 changes
