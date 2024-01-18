! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/F2003/deferlen/functional/allocate/deferLenAllocate022.f
! opt variations: -qnock -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with LLE, LLT
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

   character(:), pointer :: c1

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), pointer :: c
   end type

   type(base(4,20)) :: b1

end module

program deferLenAllocate022
   use m

   ! same

   allocate ( c1, source = "abcdefghijklmnopqrstuvwxyz" )
   allocate ( b1%c, source = "abcdefghijklmnopqrstuvwxyz" )
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)

   ! same but different case

   deallocate ( c1, b1%c )

   allocate ( c1, source = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
   allocate ( b1%c, source = "abcdefghijklmnopqrstuvwxyz" )
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)

   ! different

   deallocate ( c1, b1%c )

   allocate ( c1, source = "dogs" )
   allocate ( b1%c, source = "cats" )
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)

end program
