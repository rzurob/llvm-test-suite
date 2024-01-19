! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/functional/allocate/deferLenAllocate020.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with index
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

   character(:), allocatable :: c1

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), allocatable :: c(:)
   end type

   type(base(4,20)) :: b1

end module

program deferLenAllocate020
   use m

   allocate ( c1, source = "xxxxxxxxxxxxxxxxxxxxxxxibmxxxxxxibmx" )

   print *, len(c1), index(c1, 'ibm'), index(substring="ibm", string=c1, back=.true. )

   allocate ( b1%c(3), source = (/ "a" // c1 // "b", "c" // c1 // "d", "e" // c1 // "f"/) )

   print *, len(b1%c), index(b1%c, 'ibm'), index(b1%c, c1), index(c1,b1%c)

   deallocate ( c1 )

   allocate ( c1, source = b1%c(1) )

   print *, len(c1), index(c1, 'ibm'), index(substring=c1, string=b1%c), index(substring=b1%c, string=c1)

end program
