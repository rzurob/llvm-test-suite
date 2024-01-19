! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/functional/allocate/deferLenAllocate018.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with adjustl and adjustr
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

   character(len=: ), allocatable :: c1(:)

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), allocatable :: c(:)
   end type

   type (base(4,20)) :: b1

end module

program deferLenAllocate018
   use m

   allocate ( c1(5), source = (/ 'abc  ', 'def  ', 'ghi  ', 'jkl  ', 'mno  ' /) )

   if ( len(c1) /= 5 ) error stop 1_4

   allocate ( b1%c(5), source = adjustr(c1) )
   if ( len(b1%c) /= 5 ) error stop 2_4

   print *, c1, "|"
   print *, b1%c, "|"

   deallocate ( c1 )

   allocate ( c1(5), source = adjustl(b1%c) )
   print *, c1, "|"
   print *, b1%c, "|"

   deallocate ( c1, b1%c )

   allocate ( c1(1), source = (/ adjustr( "<-11 spaces here          " ) /) )
   print *, c1

   allocate ( b1%c(1), source = (/ adjustl ( "          10 spaces here->" ) /) )
   print *, b1%c,"|"

end program
