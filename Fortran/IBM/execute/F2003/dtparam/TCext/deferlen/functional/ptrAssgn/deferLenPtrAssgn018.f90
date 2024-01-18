! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/functional/ptrAssgn/deferLenPtrAssgn018.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
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

   character(len=: ), pointer :: c1(:)

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), pointer :: c(:)
   end type

   type (base(4,20)) :: b1

end module

program deferLenPtrAssgn018
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

   allocate ( c1(1), source = (/ adjustr( "FORTRAN 2003 is fun!          " ) /) )
   print *, c1

   b1%c => c1
   c1 = adjustl(b1%c)

   print *, c1
   print *, b1%c

end program
