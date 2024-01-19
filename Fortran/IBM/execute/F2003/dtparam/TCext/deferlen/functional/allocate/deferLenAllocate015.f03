! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/deferlen/functional/allocate/deferLenAllocate015.f
! opt variations: -qnok -ql

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with C_CHAR type
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
   use ISO_C_BINDING, only: C_CHAR

   character(kind=C_CHAR, len=: ), allocatable :: c1

   type base(k1)    ! (4)
       integer, kind :: k1
      character(kind=C_CHAR, len=: ), allocatable :: c
   end type

end module

program deferLenAllocate015
   use m

   character(kind=C_CHAR, len=: ), pointer :: c2
   type(base(4)), allocatable :: b1

   allocate ( c1, source = C_CHAR_'abcdefghij' )
   allocate ( c2, source = C_CHAR_'klmnopqrstuvwxyz' )

   print *, len(c1), c1
   print *, len(c2), c2

   allocate ( b1, source = base(4)(c1) )
   print *, len(b1%c), b1%c
   deallocate ( b1 )

   allocate ( b1, source = base(4)(C_CHAR_'klmnopqrstuvwxyz') )
   print *, len(b1%c), b1%c
   deallocate ( b1 )

   allocate ( b1, source = base(4)(c1//c2//c1//C_CHAR_'klmnopqrstuvwxyz'//c1//c2) )
   print *, len(b1%c), b1%c
   deallocate ( b1 )

end program
