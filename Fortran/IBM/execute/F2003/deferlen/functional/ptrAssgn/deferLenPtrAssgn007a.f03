!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : array character with deferred length as structure component with
!*                               implicit pointer assignment
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

module n
   type base
      character(:), pointer :: c(:)
   end type

end module

program deferLenPtrAssgn007a
   use n

   type(base), pointer :: b1
   type(base), pointer :: b2

   character, target :: a(4)=(/ "A", "B", "C", "D" /)

   character(3), target :: bbb(3)=(/ "bbb", "BBB", "BbB" /)

   allocate ( b1, source = base(a) )
   print *, b1%c, len(b1%c), size(b1%c), associated(b1%c, a)

   b2 => b1
   b2%c = a((/4,3,2,1/))
   print *, b2%c, b1%c, len(b2%c), size(b2%c), associated(b1%c, b2%c), associated( b2%c, a )

   allocate ( b2, source = base(bbb) )
   print *, b2%c, len(b2%c), size(b2%c), associated(b2%c, bbb)

   b1 => b2
   b1%c = bbb(3:1:-1)
   print *, b1%c, b2%c,len(b1%c), size(b1%c), associated(b1%c, b2%c), associated( b1%c, bbb )

!   nullify( b2%c )
   nullify( b2 )

   associate ( ggg => b1 )
      b2 => ggg
      print *, b2%c, len(b2%c), size(b2%c), associated(b2%c, b1%c), associated( b2%c, bbb )
   end associate

end program