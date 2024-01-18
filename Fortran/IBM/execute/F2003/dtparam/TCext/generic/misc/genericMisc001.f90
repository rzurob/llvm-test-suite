! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/generic/misc/genericMisc001.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : misc.
!*
!*  DESCRIPTION                : Structure Constructor for derived type
!*                               containing allocatable component and allocate it with
!*                               allocatable function result
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

  type inner(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: j = -999
      contains
         procedure :: iadd
   end type

   type base(n2,k2)    ! (20,4)
      integer, kind                  :: k2
      integer, len                   :: n2
      integer(k2)                    :: x = -999
      type(inner(:,k2)), allocatable :: i
   end type

   contains

      type(inner(20,4)) function iadd ( a, b ) result(abc)
         class(inner(*,4)), intent(in) :: a, b
         allocatable :: abc
         allocate ( abc )
         abc%j = a%j + b%j
      end function

end module

program genericMisc001
   use m

   class(base(:,4)), allocatable :: b1
   type(inner(20,4)) :: i1, i2

   i1%j = 100
   i2%j = 200

   associate ( g => base(20,4) ( x = 10, i = iadd( i1 , i2 ) ) )
      if ( .not. allocated(g%i) ) error stop 1_4
      if ( ( g%x /= 10 ) .or. ( g%i%j /= 300 ) ) error stop 2_4
   end associate

end
