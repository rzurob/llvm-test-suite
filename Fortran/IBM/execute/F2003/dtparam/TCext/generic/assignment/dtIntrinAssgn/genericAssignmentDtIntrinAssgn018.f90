! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=none /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn018.f
! opt variations: -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=self -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - if component of expr is allocated, component
!*                                      of variable is allocated with the same dynamic type
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
   end type base

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j = -999
   end type

   type, extends(child) :: gen3(n3,k3)    ! (20,4,20,4,20,4)
      integer, kind :: k3
      integer, len  :: n3
      integer(k3)   :: k = -999
   end type

   type container(k4,n4)    ! (4,20)
      integer, kind                   :: k4
      integer, len                    :: n4
      class(base(n4,k4)), allocatable :: inn0
   end type

end module

program genericAssignmentDtIntrinAssgn018
   use m

   type(container(4,20)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   c1 = container(4,20)(base(20,4)(100))

   allocate ( c2, c3 )

   if ( .not. same_type_as(c1%inn0,base(20,4)() ) ) error stop 1_4

   c2 = c1
   c3 = c2

   if ( ( .not. same_type_as(c2%inn0,base(20,4)()) ) .or. ( c2%inn0%i /= 100 ) ) error stop 2_4
   if ( ( .not. same_type_as(c3%inn0,base(20,4)()) ) .or. ( c3%inn0%i /= 100 ) ) error stop 3_4

   c2 = container(4,20)(child(20,4,20,4)(200,300))

   c1 = c2
   c3 = c2

   select type ( g => c1%inn0 )
      type is ( child(*,4,*,4) )
         if ( ( g%i /= 200 ) .or. ( g%j /= 300 ) ) error stop 4_4
      class default
         error stop 5_4
   end select

   select type ( g => c3%inn0 )
      type is ( child(*,4,*,4) )
         if ( ( g%i /= 200 ) .or. ( g%j /= 300 ) ) error stop 6_4
      class default
         error stop 7_4
   end select

   c3 = container(4,20)(gen3(20,4,20,4,20,4)(400,500,600))

   c1 = c3
   c2 = c3
   c3 = c3

   select type ( g => c1%inn0 )
      type is ( gen3(*,4,*,4,*,4) )
         if ( ( g%i /= 400 ) .or. ( g%j /= 500 ) .or. ( g%k /= 600 ) ) error stop 8_4
      class default
         error stop 9_4
   end select

   select type ( g => c2%inn0 )
      type is ( gen3(*,4,*,4,*,4) )
         if ( ( g%i /= 400 ) .or. ( g%j /= 500 ) .or. ( g%k /= 600 ) ) error stop 10_4
      class default
         error stop 11_4
   end select

   select type ( g => c3%inn0 )
      type is ( gen3(*,4,*,4,*,4) )
         if ( ( g%i /= 400 ) .or. ( g%j /= 500 ) .or. ( g%k /= 600 ) ) error stop 12_4
      class default
         error stop 13_4
   end select

end program
