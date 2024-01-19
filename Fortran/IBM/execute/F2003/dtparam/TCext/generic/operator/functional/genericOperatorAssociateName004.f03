! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/generic/operator/functional/genericOperatorAssociateName004.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: use associate name and see if
!*                                         operator of generic tb can be used for component
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

   type dt(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)      i
      contains
         procedure :: adddt
         generic :: operator ( + ) => adddt
   end type

   type base(k2,n2)    ! (4,20)
      integer, kind   :: k2
      integer, len    :: n2
      type(dt(n2,k2)) :: d
      contains
         procedure :: add
         generic :: operator ( + ) => add
   end type

   contains

      type(base(4,20)) function add ( a, b )
         class(base(4,*)), intent(in) :: a, b

         add%d = a%d + b%d

      end function

      type(dt(20,4)) function adddt ( a, b )
         class(dt(*,4)), intent(in) :: a, b

         adddt%i = a%i + b%i

      end function

end module

program genericOperatorAssociateName004
   use m

   class(base(4,:)), allocatable :: b1, b2
   type(base(4,20)) :: b3

   allocate ( b1, source = base(4,20) (dt(20,4) ( 10 )) )
   allocate ( b2, source = base(4,20) (dt(20,4) ( 20 )) )

   associate ( g => b1 , h => b2 )
      b3 = g + h
      print *, b3
      b3%d%i = 0
      associate ( g => b1%d , h => b2%d )
         b3%d = g + h
         print *, b3
      end associate
   end associate

   select type ( d => b1 )
      class default
         b3 = d + b2
         print *, b3
         select type ( e => b2 )
            class default
               b3 = e + d + e + d
               print *, b3
         end select
   end select

end program
