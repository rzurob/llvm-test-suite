! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of poly type child with kind and len
!*                               TO is of poly type base with len
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base ( l)
       integer, len :: l
       character(l), allocatable :: ch1
       character(:), allocatable :: ch2
   end type

   type, extends(base) :: child(k)
       integer, kind :: k
       character(l) :: ch(k)
       integer(k+1), allocatable :: id(:)
   end type

end module

use m

   class(child(k=3,l=8)), allocatable ::  c1
   class(base(8)), allocatable :: b1

   allocate(c1, source = child(l=8,k=3)('May ship', ' IBM ', &
         (/ 'compiler', ' XLC80  ', 'XLF11.1 ' /), (/ 2007_8, 05_8/) ))

   allocate( child(l=8, k=7) :: b1 )


   call move_alloc(c1, b1)

   if ( .not. allocated(b1) ) error stop 21
   if ( allocated(c1) ) error stop 23

   select type (b1)
       type is (child(l=*,k=3))
            print *, b1%ch1, b1%ch2, b1%ch, b1%id
   end select

end
