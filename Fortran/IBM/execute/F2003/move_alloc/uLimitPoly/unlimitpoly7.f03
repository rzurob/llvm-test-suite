! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/30/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               TO and FROM have dimension attribute
!*                               rank = 2
!*                               FROM is a component of a derived-type
!*                               dynamic type is integer
!*
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

   type base
       class(*), allocatable, dimension(:,:) :: from
   end type

end module

program main
use m

   type(base) b1

   integer i

   class(*), allocatable, dimension(:,:) :: to

   allocate(integer::b1%from(3,4))

   if ( .not. allocated(b1%from) ) error stop 21

   select type( arg =>b1%from)
       type is (integer)
            arg = reshape( (/ (i, i = -12, -1 ) /),(/3,4/) )
       class default
            stop 11
   end select

   call move_alloc( b1%from, to )

   if ( .not. allocated(to) ) error stop 23

   if ( allocated(b1%from) ) error stop 25

   select type(to)
       type is (integer)
            print *, shape(to)
            print *, to
       class default
            stop 31
   end select

end