! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/deferlen/functional/argAsso/deferLenArgAsso014.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

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
!*  DESCRIPTION                : array of character with deferred length
!*                               in user-defined assignment
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
      character(:), allocatable :: c(:)
   end type

   interface !assignment(=)
      subroutine mycharassignment(a,b)
         import base
         type(base(4,*)), intent(out)  :: a
         character(:), allocatable, intent(in) :: b(:)
      end subroutine
   end interface

end module

subroutine mycharassignment(a,b)
   use n, only: base
   type(base(4,*)), intent(out)  :: a
   character(:), allocatable, intent(in) :: b(:)

   if( allocated( a%c ) ) deallocate ( a%c )
   print *, "inside assignment: ", b, len(b), size(b)
   allocate ( a%c(size(b)), source = b )

end subroutine

program deferLenArgAsso014
   use n

   type(base(4,:)), pointer :: b1
   character(:), allocatable :: c1(:)

   allocate ( b1, source = base(4,20)(null()) )
   allocate ( c1(5), source = (/ 'abc', 'def', 'ghi', 'jkl', 'mno' /) )

   if (allocated(b1%c)) error stop 10_4

!   b1 = c1

   call mycharassignment(b1, c1)

   print *, b1%c, len(b1%c), size(b1%c)

end program
