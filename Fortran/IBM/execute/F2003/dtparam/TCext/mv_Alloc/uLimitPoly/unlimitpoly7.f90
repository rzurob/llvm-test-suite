! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpoly7.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpoly7.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 05/30/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               TO and FROM have dimension attribute
!*                               rank = 2
!*                               FROM is a component of a derived-type
!*                               dynamic type is integer
!*
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

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
       class(*), allocatable, dimension(:,:) :: from
   end type

end module

program main
use m

   type(base(4,20)) b1

   integer i 

   class(*), allocatable, dimension(:,:) :: to

   allocate(integer::b1%from(3,4))

   if ( .not. allocated(b1%from) ) stop 21

   select type( arg =>b1%from)
       type is (integer)
            arg = reshape( (/ (i, i = -12, -1 ) /),(/3,4/) )
       class default
            stop 11 
   end select

   call move_alloc( b1%from, to )
  
   if ( .not. allocated(to) ) stop 23

   if ( allocated(b1%from) ) stop 25

   select type(to)
       type is (integer)
            print *, shape(to) 
            print *, to
       class default
            stop 31 
   end select

end
