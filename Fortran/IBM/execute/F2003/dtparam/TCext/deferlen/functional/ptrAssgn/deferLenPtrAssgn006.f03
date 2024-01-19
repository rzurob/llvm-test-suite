! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/functional/ptrAssgn/deferLenPtrAssgn006.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length as structure component with
!*                               implicit poitner assignment through derived type intrinsic assignment
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
   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), pointer :: c
   end type

end module

program deferLenPtrAssgn006
   use n

   type base1(k2,n2)    ! (4,20)
       integer, kind :: k2
       integer, len  :: n2
      character(kind=1,len=:), pointer :: c
   end type

   type(base(4,20))  :: b1
   type(base1(4,20)) :: b2

   character(0), target :: c0
   character(10), target :: c10 = "abcdefghij"
   character(50), target :: c50 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

   b1 = base(4,20)( c0 )
   b2 = base1(4,20)( c10 ) ! 10 characters

   print *, b1%c, len(b1%c), associated(b1%c, c0)
   print *, b2%c, len(b2%c), associated(b2%c, c10)

   b1 = base(4,20)( c50 ) ! 50 characters
   b2 = base1(4,20)( c10(4:5) ) ! 2 characters

   print *, b1%c, len(b1%c), associated(b1%c, c50)
   print *, b2%c, len(b2%c), associated(b2%c, c10)

end program
