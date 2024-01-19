! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar017.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic binding containing dummy args of different ranks
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
      integer(k1)   :: j
      contains
         procedure, pass, private :: returnBase
         procedure, pass, private :: returnBaseArray
         generic :: return => returnBase, returnBaseArray
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: k
   end type

   contains

   class(base(4)) function returnBase(a)
      class(base(4)), intent(in) :: a
      allocatable :: returnBase

      allocate ( returnBase, source = a )
      print *,'returnbase'

   end function

   class(base(4)) function returnBaseArray(a,j)
      class(base(4)), intent(in) :: a
      integer, intent(in) :: j
      allocatable :: returnBaseArray(:)

      allocate ( returnBaseArray(j), source = (/ (a, i = 1, j ) /) )
      print *,'returnbasearray'
   end function

end module

program genericGenericNameScalar017
   use m

   class(base(4)), pointer :: b1, b3(:)
   class(base(4)), allocatable :: b2

   class(child(4)), pointer :: c1

   allocate ( b1, source = base(4)(100))
   allocate ( b2 , source = b1%return() )

   print *, b1%j
   print *, b2%j

   allocate ( b3(5), source = b1%return(5) )

   print *, b3%j

   deallocate ( b2, b3 )

   allocate ( c1, source = child(4)(50, 100) )
   print *, c1%j, c1%k

   allocate ( b2, source = c1%return() )
   select type ( b2 )
      type is ( child(4) )
         print *, b2%j, b2%k
   end select

   allocate ( b3(10), source = c1%return(10) )
   select type ( b3 )
      type is ( child(4) )
         print *, b3%j
         print *, b3%k
   end select

end program
