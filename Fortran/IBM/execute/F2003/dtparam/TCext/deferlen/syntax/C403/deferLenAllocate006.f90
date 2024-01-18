! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/functional/allocate/deferLenAllocate006.f
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
!*  DESCRIPTION                : scalar character with deferred length as structure component with
!*                               implicit allocate through intrinsic assignment
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
      character(:), allocatable :: c
   end type

end module

program deferLenAllocate006
   use n

   type base1(k2,n2)    ! (4,20)
       integer, kind :: k2
       integer, len  :: n2
      character(kind=1,len=:), allocatable :: c
   end type

   type(base(4,20))  :: b1
   type(base1(4,20)) :: b2

   b1 = base(4,20)( 'abcdefghi' )
   b2 = base1(4,20)( 'aaaaaaaaaaaaaaaaaaaaaaaaa' ) ! 25 characters

   print *, b1%c, len(b1%c)
   print *, b2%c, len(b2%c)

   b1 = base(4,20)( b2%c // 'abcdefghi' ) ! 34 characters
   b2 = base1(4,20)( b1%c // 'aaaaaaaaaaaaaaaaaaaaaaaaa' ) ! 59 characters

   print *, b1%c, len(b1%c)
   print *, b2%c, len(b2%c)

end program
